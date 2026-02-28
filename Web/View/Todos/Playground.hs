module Web.View.Todos.Playground where

import Web.View.Prelude

data PlaygroundView = PlaygroundView

instance View PlaygroundView where
    html PlaygroundView = [hsx|
        <div class="header mb-3">
            <h1>HTMX Navigation Playground</h1>
            <p class="mb-0">
                Use this page to verify boosted navigation with <code>helpers-htmx.js</code>.
            </p>
        </div>

        <div class="card p-3 mb-3">
            <h2 class="h6 mb-3">Navigation Examples</h2>
            <div class="d-flex gap-2 flex-wrap">
                <a class="btn btn-primary btn-sm" href={pathTo TodosHtmxAction}>Go To Todos Page</a>
                <a class="btn btn-outline-primary btn-sm" href={pathTo TodosPlaygroundAction}>Reload This Page</a>
                <a class="btn btn-outline-primary btn-sm" href={pathTo HelpersHtmxSpecAction}>Open helpers-htmx Lab</a>
                <button type="button" class="btn btn-outline-secondary btn-sm js-back">Back</button>
            </div>
        </div>

        <div class="card p-3 mb-3">
            <h2 class="h6 mb-3">Boosted GET Form Example</h2>
            <form method="GET" action={pathTo TodosHtmxAction} hx-push-url="true" class="row g-2 align-items-end">
                <div class="col-sm-8 col-md-6">
                    <label class="form-label" for="playground-query">Jump to todos with filter</label>
                    <input id="playground-query" name="q" class="form-control" placeholder="e.g. milk" />
                </div>
                <div class="col-sm-auto">
                    <button class="btn btn-outline-primary" type="submit">Open Todos</button>
                </div>
            </form>
        </div>

        <div class="card p-3">
            <h2 class="h6 mb-3">Expected Behavior</h2>
            <ul class="mb-0">
                <li>Links and forms are boosted by HTMX from the layout root.</li>
                <li>Only <code>#page-content</code> is swapped using morphdom.</li>
                <li>Browser URL updates on navigation and on forms with <code>hx-push-url="true"</code>.</li>
                <li>The <code>Back</code> button uses the helper behavior from <code>helpers-htmx.js</code>.</li>
            </ul>
        </div>
    |]
