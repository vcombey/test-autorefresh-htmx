module Web.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data TodosController
    = TodosHtmxAction
    | TodosPlaygroundAction
    | DebugAutoRefreshAction
    | DebugAutoRefreshTriggerAction
    | DebugAutoRefreshResetAction
    | HelpersHtmxSpecAction
    | TodoStatsFragmentAction
    | TodoListFragmentAction
    | HelpersHtmxMorphdomAction
    | HelpersHtmxEventSwapAction
    | HelpersHtmxScrollSwapAction
    | HelpersHtmxFlatpickrSwapAction
    | HelpersHtmxPingAction
    | HelpersHtmxAlertSubmitAction
    | HelpersHtmxDeleteAction
    | CreateTodoHtmxAction
    | ToggleTodoHtmxAction { todoId :: !(Id Todo) }
    | DeleteTodoHtmxAction { todoId :: !(Id Todo) }
    deriving (Eq, Show, Data)
