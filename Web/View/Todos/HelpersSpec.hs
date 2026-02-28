module Web.View.Todos.HelpersSpec
    ( HelpersHtmxSpecView (..)
    , helpersMorphdomFixture
    , helpersEventSwapFixture
    , helpersScrollSwapFixture
    , helpersFlatpickrSwapFixture
    , helpersPingFixture
    , helpersAlertSubmitFixture
    , helpersDeleteFixture
    ) where

import Web.View.Prelude

data HelpersHtmxSpecView = HelpersHtmxSpecView

instance View HelpersHtmxSpecView where
    html HelpersHtmxSpecView =
        let morphdomSwapPath = pathTo HelpersHtmxMorphdomAction <> "?variant=two"
            deletePath = pathTo HelpersHtmxDeleteAction
         in [hsx|
            <div class="header mb-3">
                <h1>helpers-htmx Parity Lab</h1>
                <p class="mb-0">
                    Runs browser checks for the helper compatibility features documented in the guide.
                </p>
                <div class="d-flex gap-2 flex-wrap mt-3">
                    <a class="btn btn-outline-primary btn-sm" href={pathTo TodosHtmxAction}>Todos Home</a>
                    <a class="btn btn-outline-primary btn-sm" href={pathTo TodosPlaygroundAction}>Navigation Playground</a>
                    <button id="run-helpers-htmx-tests" type="button" class="btn btn-primary btn-sm">
                        Run helpers-htmx checks
                    </button>
                    <span id="helpers-htmx-tests-summary" class="badge bg-secondary align-self-center">Not run</span>
                </div>
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">Results</h2>
                <ul id="helpers-htmx-test-results" class="list-group list-group-flush"></ul>
                <p class="text-muted small mb-0 mt-3">
                    Refresh the page before running the checks again.
                </p>
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">Back + Toggle Helpers</h2>
                <div class="d-flex gap-2 flex-wrap mb-3">
                    <button id="helpers-back-button" type="button" class="btn btn-outline-secondary btn-sm js-back">
                        js-back
                    </button>
                    <button id="helpers-data-back-button" type="button" class="btn btn-outline-secondary btn-sm" data-js-back="true">
                        data-js-back
                    </button>
                </div>

                <div class="form-check mb-2">
                    <input
                        id="helpers-toggle-control"
                        class="form-check-input"
                        type="checkbox"
                        data-toggle="#helpers-toggle-target"
                    />
                    <label class="form-check-label" for="helpers-toggle-control">
                        Enable dependent field
                    </label>
                </div>
                <input
                    id="helpers-toggle-target"
                    class="form-control form-control-sm"
                    placeholder="Enabled by data-toggle"
                    disabled="disabled"
                />
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">File Preview + Date/Time Formatting</h2>
                <div class="row g-3">
                    <div class="col-md-6">
                        <label class="form-label" for="helpers-file-input">File preview input</label>
                        <input
                            id="helpers-file-input"
                            class="form-control"
                            type="file"
                            accept="image/*"
                            data-preview="#helpers-preview-image"
                        />
                        <img
                            id="helpers-preview-image"
                            alt="preview"
                            style="max-width: 120px; max-height: 120px;"
                            class="mt-2 border rounded p-1"
                        />
                    </div>

                    <div class="col-md-6">
                        <div class="small text-muted mb-2">Date/time helper outputs</div>
                        <time id="helpers-timeago" class="time-ago d-block" datetime="2026-02-20T10:30:00Z"></time>
                        <time id="helpers-datetime" class="date-time d-block" datetime="2026-02-20T10:30:00Z"></time>
                        <time id="helpers-date" class="date d-block" datetime="2026-02-20T10:30:00Z"></time>
                        <time id="helpers-time" class="time d-block" datetime="2026-02-20T10:30:00Z"></time>
                    </div>
                </div>
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">Flatpickr Init + HTMX Load</h2>
                <div class="row g-3 mb-3">
                    <div class="col-md-6">
                        <label class="form-label" for="helpers-flatpickr-date">Initial date input</label>
                        <input id="helpers-flatpickr-date" type="date" class="form-control" value="2026-02-20" />
                    </div>
                    <div class="col-md-6">
                        <label class="form-label" for="helpers-flatpickr-datetime">Initial datetime input</label>
                        <input id="helpers-flatpickr-datetime" type="datetime-local" class="form-control" value="2026-02-20T10:30" />
                    </div>
                </div>

                <div id="helpers-flatpickr-swap-root">
                    <button
                        id="helpers-flatpickr-swap-trigger"
                        type="button"
                        class="btn btn-outline-primary btn-sm"
                        hx-get={pathTo HelpersHtmxFlatpickrSwapAction}
                        hx-select="unset"
                        hx-target="#helpers-flatpickr-swap-root"
                        hx-swap="innerHTML"
                    >
                        Load swapped date fields
                    </button>
                </div>
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">Event + Scroll Fixtures</h2>
                <div class="d-flex gap-2 flex-wrap mb-3">
                    <div id="helpers-event-swap-root">
                        <button
                            id="helpers-event-swap-trigger"
                            type="button"
                            class="btn btn-outline-primary btn-sm"
                            hx-get={pathTo HelpersHtmxEventSwapAction}
                            hx-select="unset"
                            hx-target="#helpers-event-swap-root"
                            hx-swap="innerHTML"
                        >
                            Swap event fixture
                        </button>
                    </div>

                    <div id="helpers-scroll-swap-root">
                        <button
                            id="helpers-scroll-swap-trigger"
                            type="button"
                            class="btn btn-outline-primary btn-sm"
                            hx-get={pathTo HelpersHtmxScrollSwapAction}
                            hx-select="unset"
                            hx-target="#helpers-scroll-swap-root"
                            hx-swap="innerHTML"
                        >
                            Swap scroll fixture
                        </button>
                    </div>
                </div>
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">Alert Dismiss + Delete Confirmation</h2>
                <form
                    id="helpers-alert-form"
                    method="POST"
                    hx-post={pathTo HelpersHtmxAlertSubmitAction}
                    hx-select="unset"
                    hx-target="#helpers-alert-result"
                    hx-swap="innerHTML"
                >
                    <div id="helpers-alert" class="alert alert-warning mb-2">This alert should dismiss on request.</div>
                    <button id="helpers-alert-submit" class="btn btn-outline-primary btn-sm" type="submit" hx-disabled-elt="#helpers-alert-submit">
                        Submit request
                    </button>
                </form>
                <div id="helpers-alert-result" class="small mt-2"></div>

                <div id="helpers-delete-container" class="mt-3">
                    <button
                        id="helpers-delete-button"
                        type="button"
                        class="btn btn-outline-danger btn-sm"
                        hx-post={deletePath}
                        hx-confirm="Confirm demo request?"
                        hx-select="unset"
                        hx-target="#helpers-delete-result"
                        hx-swap="innerHTML"
                    >
                        Trigger hx-confirm request
                    </button>
                    <div id="helpers-delete-result" class="small mt-2"></div>
                </div>
            </div>

            <div class="card p-3 mb-3">
                <h2 class="h6 mb-3">Morphdom Swap Fixture</h2>
                <div id="helpers-morphdom-target">
                    {helpersMorphdomFixture "one"}
                </div>
                <button
                    id="helpers-morphdom-swap-trigger"
                    type="button"
                    class="btn btn-outline-primary btn-sm mt-2"
                    hx-get={morphdomSwapPath}
                    hx-select="unset"
                    hx-target="#helpers-morphdom-target"
                    hx-swap="morphdom"
                >
                    Swap to variant two
                </button>
                <div id="helpers-process-result" class="small mt-2"></div>
            </div>

            <script src={assetPath "/helpers-htmx-spec.js"}></script>
        |]

helpersMorphdomFixture :: Text -> Html
helpersMorphdomFixture variant =
    let dynamicButton = [hsx|
            <button
                id="helpers-dynamic-hx-button"
                type="button"
                class="btn btn-outline-secondary btn-sm mt-3"
                hx-get={pathTo HelpersHtmxPingAction}
                hx-select="unset"
                hx-target="#helpers-process-result"
                hx-swap="innerHTML"
            >
                Run dynamic hx-get
            </button>
        |]
        defaultValue :: Text
        defaultValue = if variant == "one" then "server-one" else "server-two"
     in [hsx|
        <div id="helpers-morphdom-target-inner">
            <div id="helpers-morph-version" data-version={variant} class="small text-muted mb-2">
                Morphdom variant: {variant}
            </div>

            <div id="helpers-node-keyed" class="mb-2">Node identity anchor</div>

            <div class="row g-2">
                <div class="col-md-4">
                    <label class="form-label" for="helpers-morph-input">Input value preservation</label>
                    <input
                        id="helpers-morph-input"
                        class="form-control form-control-sm"
                        value={defaultValue}
                    />
                </div>

                <div class="col-md-4">
                    <label class="form-label" for="helpers-morph-check">Checkbox preservation</label>
                    <div class="form-check">
                        <input
                            id="helpers-morph-check"
                            class="form-check-input"
                            type="checkbox"
                            checked={variant == "two"}
                        />
                    </div>
                </div>

                <div class="col-md-4">
                    <label class="form-label" for="helpers-morph-select">Select preservation</label>
                    <select id="helpers-morph-select" class="form-select form-select-sm">
                        <option value="a" selected={variant == "one"}>A</option>
                        <option value="b" selected={variant == "two"}>B</option>
                    </select>
                </div>
            </div>

            {if variant == "two" then dynamicButton else mempty}
        </div>
    |]

helpersEventSwapFixture :: Html
helpersEventSwapFixture = [hsx|
    <div id="helpers-event-marker" data-swap="done" class="small text-success">
        Event swap completed.
    </div>
|]

helpersScrollSwapFixture :: Html
helpersScrollSwapFixture = [hsx|
    <div id="helpers-scroll-target" class="js-scroll-into-view small text-success">
        Scroll target loaded.
    </div>
|]

helpersFlatpickrSwapFixture :: Html
helpersFlatpickrSwapFixture = [hsx|
    <div id="helpers-flatpickr-swap-fragment" class="row g-2">
        <div class="col-md-6">
            <label class="form-label" for="helpers-flatpickr-swapped-date">Swapped date input</label>
            <input id="helpers-flatpickr-swapped-date" type="date" class="form-control" value="2026-02-21" />
        </div>
        <div class="col-md-6">
            <label class="form-label" for="helpers-flatpickr-swapped-datetime">Swapped datetime input</label>
            <input id="helpers-flatpickr-swapped-datetime" type="datetime-local" class="form-control" value="2026-02-21T08:45" />
        </div>
    </div>
|]

helpersPingFixture :: Html
helpersPingFixture = [hsx|<span id="helpers-ping-result">pong</span>|]

helpersAlertSubmitFixture :: Html
helpersAlertSubmitFixture = [hsx|<span id="helpers-alert-response">submitted</span>|]

helpersDeleteFixture :: Html
helpersDeleteFixture = [hsx|<span id="helpers-delete-response">delete-ok</span>|]
