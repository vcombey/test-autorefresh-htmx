module Web.Controller.TodosController where

import qualified Data.Text as Text
import IHP.AutoRefresh
import qualified IHP.ViewSupport as ViewSupport
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as WAI
import Web.Controller.Prelude
import Web.View.Todos.Fragments.List
import Web.View.Todos.Fragments.Stats
import Web.View.Todos.Htmx
import Web.View.Todos.Playground

instance Controller TodosController where
    action TodosHtmxAction = do
        let searchQuery = paramOrDefault @Text "" "q"
        let newTodo = newRecord @Todo
        render HtmxView { .. }

    action TodosPlaygroundAction = do
        render PlaygroundView

    action TodoListFragmentAction = do
        let searchQuery = paramOrDefault @Text "" "q"
        autoRefresh do
            let baseQuery = query @Todo |> orderByDesc #createdAt
            todos <- baseQuery |> fetch
            let normalizedSearch = searchQuery |> Text.toLower
            let visibleTodos =
                    if searchQuery == ""
                        then todos
                        else todos
                            |> filter (\todo -> normalizedSearch `isInfixOf` (todo.title |> Text.toLower))
            let view = TodoListFragmentView { todos = visibleTodos, searchQuery }
            let ?view = view
            respondHtml (ViewSupport.html view)

    action TodoStatsFragmentAction = do
        autoRefresh do
            todos <- query @Todo |> fetch
            let totalCount = length todos
            let doneCount = todos |> filter (\todo -> todo.isDone) |> length
            let pendingCount = totalCount - doneCount
            let view = TodoStatsFragmentView { totalCount, doneCount, pendingCount }
            let ?view = view
            respondHtml (ViewSupport.html view)

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

    action ToggleTodoHtmxAction { todoId } = do
        todo <- fetch todoId
        todo
            |> set #isDone (not todo.isDone)
            |> updateRecord
        respondAndExit $ WAI.responseLBS HTTP.status204 [] ""

    action DeleteTodoHtmxAction { todoId } = do
        todo <- fetch todoId
        deleteRecord todo
        respondAndExit $ WAI.responseLBS HTTP.status204 [] ""
