module Web.Controller.TodosController where

import IHP.AutoRefresh
import IHP.AutoRefresh.ChangeSet
import IHP.Controller.Layout (setLayout)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (responseLBS)
import qualified Network.Wai as WAI
import Web.Controller.Prelude
import Web.View.Todos.Fragments.List
import Web.View.Todos.Htmx

instance Controller TodosController where
    action TodosHtmxAction = do
        let newTodo = newRecord @Todo
        render HtmxView { .. }

    action TodoListFragmentAction = do
        autoRefresh do
            todos <- query @Todo |> orderByDesc #createdAt |> fetch
            html <- renderHtml TodoListFragmentView { .. }
            respondHtml html

    action CreateTodoHtmxAction = do
        let newTodo = newRecord @Todo
        newTodo
            |> fill @'["title"]
            |> ifValid \case
                Left _ ->
                    respondAndExit $ WAI.responseLBS HTTP.status422 [] ""
                Right todo -> do
                    todo |> set #isDone False |> createRecord
                    respondAndExit $ WAI.responseLBS HTTP.status204 [] ""


