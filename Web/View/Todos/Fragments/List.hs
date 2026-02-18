module Web.View.Todos.Fragments.List where

import Web.View.Prelude

data TodoListFragmentView = TodoListFragmentView
    { todos :: [Todo]
    , searchQuery :: Text
    }

instance View TodoListFragmentView where
    html TodoListFragmentView { .. } = [hsx|
        <div class="card p-3">
            <div class="d-flex justify-content-between align-items-center mb-3">
                <h2 class="h5 mb-0">Todo List</h2>
                <div class="text-end">
                    <div class="text-muted small">{tshow (length todos)} items</div>
                    {renderFilterLabel}
                </div>
            </div>
            {listBody}
        </div>
    |]
        where
            listBody =
                if null todos
                    then [hsx|<p class="text-muted mb-0">No todos yet.</p>|]
                    else [hsx|
                        <ul class="todo-list">
                            {forEach todos renderTodoItem}
                        </ul>
                    |]

            renderTodoItem todo =
                let togglePath = pathTo ToggleTodoHtmxAction { todoId = todo.id }
                    deletePath = pathTo DeleteTodoHtmxAction { todoId = todo.id }
                 in [hsx|
                <li class={todoClass todo}>
                    <div class="d-flex justify-content-between align-items-start gap-3">
                      <div>
                        <span class="title">{todo.title}</span>
                        <small class="updated-at">Updated: {tshow todo.updatedAt}</small>
                      </div>
                      <div class="d-flex gap-2">
                        <button
                            class="btn btn-outline-secondary btn-sm"
                            hx-post={togglePath}
                            hx-swap="none"
                            hx-disabled-elt="this"
                        >
                            {if todo.isDone then ("Mark Undone" :: Text) else ("Mark Done" :: Text)}
                        </button>
                        <button
                            class="btn btn-outline-danger btn-sm"
                            hx-delete={deletePath}
                            hx-confirm="Delete this todo?"
                            hx-swap="none"
                            hx-disabled-elt="this"
                        >
                            Delete
                        </button>
                      </div>
                    </div>
                </li>
            |]

            todoClass todo =
                if todo.isDone
                    then ("todo done" :: Text)
                    else "todo"

            renderFilterLabel =
                if searchQuery == ""
                    then mempty
                    else [hsx|<div class="small">Filter: <code>{searchQuery}</code></div>|]
