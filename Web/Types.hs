module Web.Types where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data TodosController
    = TodosHtmxAction
    | TodoListFragmentAction
    | CreateTodoHtmxAction
    deriving (Eq, Show, Data)
