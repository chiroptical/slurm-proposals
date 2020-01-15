{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TupleSections    #-}

module Query.Statistic where

import           Database
import           Query.Account
import           Table.Account
import           Table.Statistic
import           Type.Backend
import           Type.Frontend

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)

selectAllStatistics ::
     Q Sqlite ProposalsDb QBaseScope (StatisticT (QExpr Sqlite QBaseScope))
selectAllStatistics = all_ (_proposalsStatistics proposalsDb)

toStatistic :: (Account_, Statistic_) -> Statistic
toStatistic (Account_ { _accountName = name
                      , _accountOwner = owner
                      , _accountDepartment = dept
                      }, Statistic_ { _statisticUnusedServiceUnits = sus
                                    , _statisticExpirationDate = end
                                    }) =
  Statistic sus end (Account name owner dept)

getStatistics ::
     MonadBeam Sqlite m => Text -> m (Either BackendError [Statistic])
getStatistics name = fmap distribute <$> getStatistics_ name
  where
    distribute (acc, ss) = toStatistic <$> zip (repeat acc) ss

getStatistics_ ::
     MonadBeam Sqlite m
  => Text
  -> m (Either BackendError (Account_, [Statistic_]))
getStatistics_ name = do
  mAccount_ <- getAccount_ name
  case mAccount_ of
    Left err -> pure . Left $ err
    Right account_ ->
      fmap (Right . (account_, )) $
      runSelectReturningList $
      select $ do
        statistic <- selectAllStatistics
        guard_ (_statisticAccount statistic ==. val_ (primaryKey account_))
        pure statistic

getStatisticById ::
     MonadBeam Sqlite m => Int -> m (Either BackendError Statistic)
getStatisticById = (fmap . fmap) toStatistic . getStatisticById_

getStatisticById_ ::
     MonadBeam Sqlite m => Int -> m (Either BackendError (Account_, Statistic_))
getStatisticById_ id = do
  mStatistic_ <-
    runSelectReturningOne $
    select $ do
      statistic <- selectAllStatistics
      guard_ (_statisticId statistic ==. val_ id)
      pure statistic
  case mStatistic_ of
    Nothing -> pure . Left $ StatisticIdDoesntExist id
    Just statistic_ -> do
      let AccountId id = _statisticAccount statistic_
      fmap (, statistic_) <$> getAccountById_ id

insertStatistic ::
     MonadBeam Sqlite m
  => Statistic
  -> m (Either BackendError (Account_, Statistic_))
insertStatistic Statistic { statisticUnusedServiceUnits = sus
                          , statisticExpirationDate = exp
                          , statisticAccount = Account {accountName = name}
                          } = do
  eAccount_ <- getAccount_ name
  case eAccount_ of
    Left err -> pure . Left $ err
    Right account_ -> do
      runInsert $
        insert (_proposalsStatistics proposalsDb) $
        insertExpressions
          [ Statistic_
              default_
              (val_ sus)
              (val_ exp)
              (val_ $ primaryKey account_)
          ]
      (fmap . fmap . fmap) last (getStatistics_ name)
