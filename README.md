# IHP Project

This is an IHP (Integrated Haskell Platform) project with GitHub Actions for testing and deployment. For more information about IHP, see the [IHP Documentation](https://ihp.digitallyinduced.com/Guide/).

## HTMX Sandbox Checks

This sandbox is configured to test HTMX + Auto Refresh with:

- `/helpers-htmx.js`
- `/ihp-auto-refresh-htmx.js`
- layout-level `hx-boost` + morphdom swap into `#page-content`

Open the app and try:

1. Navigation: click `Open Navigation Playground` and `Go To Todos Page` to verify boosted link navigation and URL updates.
2. Boosted GET form: submit filter forms (`q`) and confirm URL updates (`?q=...`) without full page reload.
3. Fragment load/reload: use `Reload List Fragment` to fetch just the todo list fragment.
4. Auto refresh after writes: create a todo, then toggle or delete one. Actions return `204`; list updates via auto refresh.
5. Helper behavior: click `Back` buttons (`.js-back`) and verify helper behavior from `helpers-htmx.js`.

### helpers-htmx parity lab

Use `/HelpersHtmxSpec` to run a feature-by-feature parity check against the guide mapping.

1. Open `/HelpersHtmxSpec`.
2. Click `Run helpers-htmx checks`.
3. Review per-feature pass/fail output in the results list.

The current suite covers:

- `.js-back` and `[data-js-back]`
- `[data-toggle]`
- file preview (`data-preview`)
- date/time formatting (`.time-ago`, `.date-time`, `.date`, `.time`)
- flatpickr init on load and after HTMX swaps
- `ihp:load` / `ihp:unload`
- `.js-scroll-into-view` after swap
- alert dismissal on request and `hx-disabled-elt`
- `hx-confirm` request flow
- morphdom parity checks (node identity, input/checkbox state, HTMX re-processing)
- timer tracking utilities (`allIntervals`, `allTimeouts`, `clearAll*`)

## GitHub Actions Workflow

This project includes a GitHub Actions workflow for automated testing and deployment. The workflow is defined in `.github/workflows/test.yml`.

### Workflow Triggers

The workflow is triggered on:
- Push to the `main` branch
- Pull requests to the `main` branch
- Manual trigger from the GitHub Actions tab

### Testing

The testing job performs the following steps:
1. Checks out the code
2. Sets up Nix
3. Initializes Cachix for faster builds
4. Installs and allows direnv
5. Builds generated files
6. Starts the project in the background
7. Runs the tests

### Deployment

For deployment, follow the [IHP Deployment Guide](https://ihp.digitallyinduced.com/Guide/deployment.html#deploying-with-deploytonixos) to set up a proper NixOS server for your project.

The deployment job runs after successful tests and only for the `main` branch. It performs the following steps:
1. Checks out the code
2. Sets up SSH for deployment
3. Sets up Nix
4. Initializes Cachix
5. Sets up direnv
6. Deploys to a NixOS server

## Setup Instructions

To use the GitHub Actions workflow in this project:

1. Set up the following secrets in your GitHub [repository settings](https://docs.github.com/en/actions/security-guides/using-secrets-in-github-actions):
   - `SSH_HOST`: The hostname or IP address of your deployment server
   - `SSH_USER`: The username for SSH access to the deployment server
   - `SSH_PRIVATE_KEY`: The private SSH key for authentication

2. Modify the `env` section in `.github/workflows/test.yml` if needed:
   - Update `PROJECT_NAME` to match your project
   - Adjust `ENV` if you want to use a different environment name
   - Update `NIXPKGS` if you want to use a different Nixpkgs version

3. Ensure your project has the necessary test files in the `Test` directory.

4. If your deployment process differs, modify the `deploy` job in the workflow file accordingly.

5. Push your changes to the `main` branch to trigger the workflow.

## Manual Workflow Trigger

You can manually trigger the workflow from the Actions tab in your GitHub repository. This is useful for running tests or deploying without pushing changes.

## Customization

Feel free to customize the workflow file to fit your specific project needs. You may want to add additional steps, change the deployment process, or modify the testing procedure.

## Support

For issues related to IHP or this project's setup, please refer to the [IHP documentation](https://ihp.digitallyinduced.com/Guide/) or seek help on the [IHP Forum](https://ihp.digitallyinduced.com/community/).

For project-specific issues, please open an issue in this repository.
