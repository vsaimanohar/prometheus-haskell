{-# LANGUAGE ExistentialQuantification #-}
module Prometheus.Registry (
    register
,   registerIO
,   unsafeRegister
,   unsafeRegisterIO
,   collectMetrics
,   unregisterAll
,   availableNamespaces
) where

import Prometheus.Metric

import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe


-- $setup
-- >>> :module +Prometheus
-- >>> unregisterAll

-- | A 'Registry' is a list of all registered metrics, currently represented by
-- their sampling functions grouped under a namespace
type Registry = [IO [SampleGroup]]

{-# NOINLINE globalRegistry #-}
globalRegistry :: STM.TVar (Map.Map T.Text Registry)
globalRegistry = unsafePerformIO $ STM.newTVarIO mempty

-- | Registers a metric with the global metric registry under a namespace.
register :: MonadIO m => T.Text -> Metric s -> m s
register ns (Metric mk) = liftIO $ do
    (metric, sampleGroups) <- mk
    let addToRegistry = (sampleGroups :)
    liftIO $ STM.atomically $ STM.modifyTVar' globalRegistry (Map.alter (Just . addToRegistry . fromMaybe mempty) ns)
    return metric

-- | Registers a metric with the global metric registry under a namespace.
registerIO :: MonadIO m => T.Text -> m (Metric s) -> m s
registerIO ns metricGen = metricGen >>= register ns

-- | Registers a metric with the global metric registry under a namespace.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
unsafeRegister :: T.Text -> Metric s -> s
unsafeRegister ns = unsafePerformIO . register ns

-- | Registers a metric with the global metric registry under a namespace.
--
-- __IMPORTANT__: This method should only be used to register metrics as top
-- level symbols, it should not be run from other pure code.
--
-- For example,
--
-- >>> :{
--  {-# NOINLINE c #-}
--  let c = unsafeRegisterIO $ counter (Info "my_counter" "An example metric")
-- :}
-- ...
unsafeRegisterIO :: T.Text -> IO (Metric s) -> s
unsafeRegisterIO ns = unsafePerformIO . registerIO ns

-- | Removes all currently registered metrics from the registry under a namespace.
unregisterAll :: MonadIO m => T.Text -> m ()
unregisterAll ns = liftIO $ STM.atomically $ STM.modifyTVar' globalRegistry (Map.delete ns)

-- | Collect samples from all currently registered metrics. In typical use cases
-- there is no reason to use this function, instead you should use
-- `exportMetricsAsText` or a convenience library.
--
-- This function is likely only of interest if you wish to export metrics in
-- a non-supported format for use with another monitoring service.
collectMetrics :: MonadIO m => T.Text -> m [SampleGroup]
collectMetrics ns = liftIO $ do
    registry <- STM.atomically $ STM.readTVar globalRegistry
    concat <$> sequence (fromMaybe mempty $ Map.lookup ns registry)

-- | All available metric namespaces at this point of time.
availableNamespaces :: MonadIO m => m [ T.Text ]
availableNamespaces =
  liftIO $
    STM.atomically $
      Map.keys <$> STM.readTVar globalRegistry
