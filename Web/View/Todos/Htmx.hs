module Web.View.Todos.Htmx where

import Web.View.Prelude

data HtmxView = HtmxView
    { newTodo :: Todo
    }

instance View HtmxView where
    html HtmxView { .. } = [hsx|
        <div class="container">
          <div class="header mb-3">
            <h1>Todos (HTMX + AutoRefresh)</h1>
            <p class="mb-0">
              Simple demo: plain HTMX form + one auto-refresh list fragment.
            </p>
          </div>

          <div class="card p-3">
            <h2 class="h6 mb-3">Add Todo</h2>
            {renderForm}
          </div>

          <div
            class="mt-3"
            id="todo-list-fragment"
            hx-get={pathTo TodoListFragmentAction}
            hx-trigger="load"
          ></div>
        </div>
    |]
        where
            renderForm =
                formForWithOptions newTodo formOptions [hsx|
                    {(textField #title) { fieldLabel = "New todo", placeholder = "Buy milk", required = True }} {submitButton { label = "Add todo" }}
                |]

            formOptions formContext =
                formContext
                    |> set #formAction (pathTo CreateTodoHtmxAction)
