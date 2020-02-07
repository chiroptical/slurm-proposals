[![badge](https://action-badges.now.sh/barrymoo/slurm-proposals)](https://github.com/barrymoo/slurm-proposals/actions)

TODO
---

Need to figure out a monad transformer stack to operate in and update database
and CLI functions to operate in it.

# slurm-proposals

A proposal system for Slurm written in Haskell. This is a work in progress and
doesn't yet function.

## Important Questions

1. For database operations should we run under transactions?

## Why?

This is a more robust replacement for [slurm-bank](https://github.com/barrymoo/slurm-bank).

The Slurm association based limits didn't provide enough power for our center's
needs. I have seen a few other banking systems but they didn't exactly work for
my needs.

We have proposals/investors which are given/purchasing service units (SUs). I
would like one system to manage both of SUs. For proposals, SUs should expire
after one year. At that time, the researcher can submit a new proposal. For
investors, SUs should expire after 5 years.

## How?

Using the existing associations in our Slurm database, we use the `RawUsage`
from `sshare` to monitor SUs on the cluster. From the documentation:


> Raw Usage
> The number of cpu-seconds of all the jobs that charged the account by the user.
> This number will decay over time when PriorityDecayHalfLife is defined
> 
> PriorityDecayHalfLife
> This controls how long prior resource use is considered in determining how
> over- or under-serviced an association is (user, bank account and cluster) in
> determining job priority. The record of usage will be decayed over time, with
> half of the original value cleared at age PriorityDecayHalfLife. If set to 0 no
> decay will be applied. This is helpful if you want to enforce hard time limits
> per association. If set to 0 PriorityUsageResetPeriod must be set to some
> interval.

Therefore, in your Slurm configuration you will need:

> PriorityDecayHalfLife=0-00:00:00
> PriorityUsageResetPeriod=NONE

The proposal system will take care of resetting `RawUsage` for you every year.
The bank will enforce two limits:

1. Is the account usage higher than their limits
2. Is the proposal past its expiration date

## Prerequisites

- GHC (likely via [Stack](https://docs.haskellstack.org/en/stable/README))
- Slurm, we use 17.11.7 

## Slurm Setup

### Charging

This is extremely important. If the charging is not set the SUs charged per job
will not mean anything. We use a MAX(CPU, Memory, GPU) charging scheme
(`PriorityFlags=MAX_TRES`). For each cluster, we define `DefMemPerCPU=<RAM in
Mb> / <cores per node>` (choosing lowest value on each cluster). Then:

- CPU Only Node: `TresBillingWeights="CPU=1.0,Mem=<value>G"` where `<value> = 1 / (DefMemPerCPU / 1024)`
- GPU Node: `TresBillingWeights="CPU=0.0,Mem=0.0G,GRES/gpu=1.0"`

Here, `CPU=1.0` means 1 SU per hour to use 1 core and `GRES/gpu=1.0` means 1 SU
per hour to use 1 GPU card. `Mem=<value>G` is defined such that for one hour,
using the default RAM, users are charged 1 SU.

## Checking and Notifications

We do necessary checks and notificatons once daily. Notifications are sent out when:

1. Has the account overdrawn their SUs? Put a hold on the account and email account owner
2. Has the proposal past the expiration date? Put a hold on the account and email account owner
3. Notifications are sent to the account owner when they exceed the following
   usage (only notifies account owner once)
  1. 90%
  2. 75%
  3. 50%
  4. 25%
