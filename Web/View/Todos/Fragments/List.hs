module Web.View.Todos.Fragments.List where

import Web.View.Prelude

data TodoListFragmentView = TodoListFragmentView
    { todos :: [Todo]
    }

instance View TodoListFragmentView where
    html TodoListFragmentView { .. } = [hsx|
        <div class="card p-3">
            <div class="d-flex justify-content-between align-items-center mb-3">
                <h2 class="h5 mb-0">Todo List</h2>
                <span class="text-muted small">{tshow (length todos)} items</span>
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

            renderTodoItem todo = [hsx|
                <li class={todoClass todo}>
                    <div>
                        <span class="title">{todo.title}</span>
                        <small class="updated-at">Updated: {tshow todo.updatedAt}</small>
                    </div>
                </li>
            |]

            todoClass todo =
                if todo.isDone
                    then ("todo done" :: Text)
                    else "todo"
