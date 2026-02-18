module Web.View.Todos.Fragments.Stats where

import Web.View.Prelude

data TodoStatsFragmentView = TodoStatsFragmentView
    { totalCount :: Int
    , doneCount :: Int
    , pendingCount :: Int
    }

instance View TodoStatsFragmentView where
    html TodoStatsFragmentView { .. } = [hsx|
        <div class="card p-3 mb-3">
            <div class="d-flex justify-content-between align-items-center">
                <h2 class="h5 mb-0">Todo Stats</h2>
                <span class="badge bg-dark">{tshow totalCount} total</span>
            </div>

            <div class="mt-3 d-flex gap-2 flex-wrap">
                <span class="badge bg-success">{tshow doneCount} done</span>
                <span class="badge bg-secondary">{tshow pendingCount} pending</span>
            </div>
        </div>
    |]
