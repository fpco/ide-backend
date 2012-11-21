{-# OPTIONS_GHC -funbox-strict-fields #-}

module GHC.RTS.Events.Merge (mergeEventLogs) where

import GHC.RTS.Events
import Data.Monoid
import Data.List (foldl')
import qualified Data.Map as M
import Data.Word (Word32, Word16)

-- TODO: add a merge mode where the events are synchronized using
-- the wall clock time event at the start of both eventlogs (for newer GHCs).
-- Such merge is not associative so we either need to take many arguments
-- or cope with eventlogs with many wall clock time events (assume they
-- are products of previous merges). To decide.

{-
GHC numbers caps and capsets in sequential order, starting at 0.  Threads are
similarly numbered, but start at 1.  In order to merge logs 'x' and 'y',
we find the # of occupied numbers for each variable type in 'x',
then increment each variable in 'y' by that amount.
We assume that if a number is occupied, so are all lower numbers.
This guarantees that variables in each log don't clash,
and that the meaning of each reference to a thread/cap/capset is
preserved.
-}

mergeEventLogs :: EventLog -> EventLog -> EventLog
mergeEventLogs (EventLog h1 (Data xs)) (EventLog h2 (Data ys)) =
  let headerMap = M.fromList . map (\ et@EventType {num} -> (num, et))
      m1 = headerMap $ eventTypes h1
      m2 = headerMap $ eventTypes h2
      combine et1 et2 | et1 == et2 = et1
      combine _ _ = error "can't merge eventlogs with inconsistent headers"
      m = M.unionWith combine m1 m2
      h = Header $ M.elems m
  in h == h `seq`  -- Detect inconsistency ASAP.
     EventLog h . Data . mergeOn time xs $ shift (maxVars xs) ys

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn f [] ys = ys
mergeOn f xs [] = xs
mergeOn f (x:xs) (y:ys) | f x <= f y = x : mergeOn f xs (y:ys)
                        | otherwise  = y : mergeOn f (x:xs) ys

-- TODO: rename, since these are not maximal values, but numbers of used values
data MaxVars = MaxVars { mcapset :: !Word32
                       , mcap :: !Int
                       , mthread :: !ThreadId }

instance Monoid MaxVars where
    mempty  = MaxVars 0 0 0
    mappend (MaxVars a b c) (MaxVars x y z) =
      MaxVars (max a x) (b + y) (max c z)
    -- avoid space leaks:
    mconcat = foldl' mappend mempty

-- Capabilities are represented as Word16.  An event block marked with the
-- capability of 0xffff belongs to no capability
isCap :: Int -> Bool
isCap x = fromIntegral x /= ((-1) :: Word16)

-- For caps we find the maximum value by summing the @Startup@ declarations.
-- TODO: it's not trivial to add CapCreate since we don't know
-- if created caps are guaranteed to be numbered consecutively or not
-- (are they? is it asserted in GHC code somewhere?). We might instead
-- just scan all events mentioning a cap and take the maximum,
-- but it's a slower and much longer code, requiring constant maintenance.
maxVars :: [Event] -> MaxVars
maxVars = mconcat . map (maxSpec . spec)
 where
    -- only checking binding sites right now, sufficient?
    maxSpec (Startup n) = mempty { mcap = n }
    -- Threads start at 1.
    maxSpec (CreateThread t) = mempty { mthread = t }
    maxSpec (CreateSparkThread t) = mempty { mthread = t }
    -- Capsets start at 0.
    maxSpec (CapsetCreate cs _) = mempty {mcapset = cs + 1 }
    maxSpec (EventBlock _ _ es) = maxVars es
    maxSpec _  = mempty

sh :: Num a => a -> a -> a
sh x y = x + y

shift :: MaxVars -> [Event] -> [Event]
shift mv@(MaxVars mcs mc mt) = map (\(Event t s) -> Event t $ shift' s)
 where
    -- -1 marks a block that isn't attached to a particular capability
    shift' (EventBlock et c bs) = EventBlock et (msh c) $ shift mv bs
     where msh x = if isCap x then sh mc x else x
    shift' (CreateThread t) = CreateThread $ sh mt t
    shift' (RunThread t) = RunThread $ sh mt t
    shift' (StopThread t s) = StopThread (sh mt t) s
    shift' (ThreadRunnable t) = ThreadRunnable $ sh mt t
    shift' (MigrateThread t c) = MigrateThread (sh mt t) (sh mc c)
    shift' (WakeupThread t c) = WakeupThread (sh mt t) (sh mc c)
    shift' (ThreadLabel t l) = ThreadLabel (sh mt t) l
    shift' (CreateSparkThread t) = CreateSparkThread (sh mt t)
    shift' (SparkSteal c) = SparkSteal (sh mc c)
    shift' (TaskCreate tk c tid) = TaskCreate tk (sh mc c) tid
    shift' (TaskMigrate tk c1 c2) = TaskMigrate tk (sh mc c1) (sh mc c2)
    shift' (CapCreate c) = CapCreate (sh mc c)  -- TODO: correct?
    shift' (CapDelete c) = CapDelete (sh mc c)  -- TODO: correct?
    shift' (CapDisable c) = CapDisable (sh mc c)
    shift' (CapEnable c) = CapEnable (sh mc c)
    shift' (CapsetCreate cs cst) = CapsetCreate (sh mcs cs) cst
    shift' (CapsetDelete cs) = CapsetDelete (sh mcs cs)
    shift' (CapsetAssignCap cs c) = CapsetAssignCap (sh mcs cs) (sh mc c)
    shift' (CapsetRemoveCap cs c) = CapsetRemoveCap (sh mcs cs) (sh mc c)
    shift' (RtsIdentifier cs rts) = RtsIdentifier (sh mcs cs) rts
    shift' (ProgramArgs cs as) = ProgramArgs (sh mcs cs) as
    shift' (ProgramEnv cs es) = ProgramEnv (sh mcs cs) es
    shift' (OsProcessPid cs pid) = OsProcessPid (sh mcs cs) pid
    shift' (OsProcessParentPid cs ppid) = OsProcessParentPid (sh mcs cs) ppid
    shift' (WallClockTime cs sec nsec) = WallClockTime (sh mcs cs) sec nsec
    shift' x = x
