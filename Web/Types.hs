module Web.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data TodosController
    = TodosHtmxAction
    | TodosPlaygroundAction
    | TodoStatsFragmentAction
    | TodoListFragmentAction
    | CreateTodoHtmxAction
    | ToggleTodoHtmxAction { todoId :: !(Id Todo) }
    | DeleteTodoHtmxAction { todoId :: !(Id Todo) }
    deriving (Eq, Show, Data)
