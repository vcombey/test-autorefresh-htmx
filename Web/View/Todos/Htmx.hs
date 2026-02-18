module Web.View.Todos.Htmx where

import Web.View.Prelude

data HtmxView = HtmxView
    { newTodo     :: Todo
    , searchQuery :: Text
    }

instance View HtmxView where
    html HtmxView { .. } = [hsx|
        <div class="header mb-3">
          <h1>Todos (HTMX + AutoRefresh)</h1>
          <p class="mb-0">
            Sandbox using boosted navigation, fragment swaps and auto refresh updates.
          </p>
          <div class="d-flex gap-2 flex-wrap mt-3">
            <a class="btn btn-outline-primary btn-sm" href={pathTo TodosHtmxAction}
              >Todos Home</a
            >
            <a class="btn btn-outline-primary btn-sm" href={pathTo TodosPlaygroundAction}
              >Open Navigation Playground</a
            >
            <button type="button" class="btn btn-outline-secondary btn-sm js-back">
              Back
            </button>
          </div>
        </div>

        <div class="card p-3 mb-3">
          <h2 class="h6 mb-3">Try These Checks</h2>
          <ol class="mb-0">
            <li>
              Click <code>Open Navigation Playground</code> and back. URL and content
              should change via HTMX (no full page reload).
            </li>
            <li>
              Submit the filter form. URL should include <code>?q=...</code> (boosted
              GET navigation).
            </li>
            <li>
              Create a todo. Response is <code>204</code>, then the list updates via
              auto refresh.
            </li>
            <li>
              Use <code>Toggle</code> and <code>Delete</code> on list items. Both list
              and stats fragments should refresh automatically.
            </li>
          </ol>
        </div>

        <div class="card p-3 mb-3">
          <h2 class="h6 mb-3">Filter Todos (boosted GET)</h2>
          <form
            method="GET"
            action={pathTo TodosHtmxAction}
            hx-push-url="true"
            class="row g-2 align-items-end"
          >
            <div class="col-sm-8 col-md-6">
              <label class="form-label" for="todo-filter-query">Title contains</label>
              <input
                id="todo-filter-query"
                name="q"
                class="form-control"
                placeholder="milk"
                value={searchQuery}
              />
            </div>
            <div class="col-sm-auto">
              <button class="btn btn-outline-primary" type="submit">
                Apply Filter
              </button>
            </div>
            <div class="col-sm-auto">
              <a class="btn btn-outline-secondary" href={pathTo TodosHtmxAction}
                >Clear</a
              >
            </div>
          </form>
        </div>

        <div class="card p-3 mb-3">
          <h2 class="h6 mb-3">Add Todo (boosted POST + auto refresh)</h2>
          {renderForm}
        </div>

        <div class="d-flex justify-content-between gap-2 flex-wrap">
          <button
            type="button"
            class="btn btn-outline-dark btn-sm"
            hx-get={pathTo TodoStatsFragmentAction}
            hx-select="unset"
            hx-swap="innerHTML"
          >
            Reload Stats Fragment
          </button>

          <button
            type="button"
            class="btn btn-outline-dark btn-sm"
            hx-get={pathTo TodoListFragmentAction}
            hx-include="#todo-filter-query"
            hx-select="unset"
            hx-swap="innerHTML"
          >
            Reload List Fragment
          </button>
        </div>

        <div
          class="mt-3"
          id="todo-stats-fragment"
          hx-get={pathTo TodoStatsFragmentAction}
          hx-trigger="load"
          hx-target="#todo-stats-fragment"
          hx-select="unset"
          hx-swap="innerHTML"
        ></div>

        <div
          class="mt-3"
          id="todo-list-fragment"
          hx-get={pathTo TodoListFragmentAction}
          hx-trigger="load"
          hx-include="#todo-filter-query"
          hx-target="#todo-list-fragment"
          hx-select="unset"
          hx-swap="innerHTML"
        ></div>
    |]
        where
            renderForm =
                formForWithOptions newTodo formOptions [hsx|
                    {(textField #title) { fieldLabel = "New todo", placeholder = "Buy milk", required = True }} {submitButton { label = "Add todo" }}
                |]

            formOptions formContext =
                formContext
                    |> set #formAction (pathTo CreateTodoHtmxAction)
