module Web.View.Layout (defaultLayout, Html) where

import Application.Helper.View
import Generated.Types
import IHP.Environment
import IHP.ViewPrelude
import Web.Routes
import Web.Types

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en">
  <head>
    {metaTags} {stylesheets} {scripts}
    <title>{pageTitleOrDefault "Todo Auto Refresh"}</title>
  </head>
  <body>
    <div class="container mt-4">
      {renderFlashMessages} {inner}
    </div>
  </body>
</html>
|]

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.2.1/bootstrap.min.css"} />
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"} />
        <link rel="stylesheet" href={assetPath "/app.css"} />
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper-2.11.6.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.2.1/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/htmx.min.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/ihp-auto-refresh-htmx.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script
          id="livereload-script"
          src={assetPath "/livereload.js"}
          data-ws={liveReloadWebsocketUrl}
        ></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8" />
    <meta
      name="viewport"
      content="width=device-width, initial-scale=1, shrink-to-fit=no"
    />
    {autoRefreshMeta}
|]
