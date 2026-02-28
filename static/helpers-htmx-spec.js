(function () {
    function byId(id) {
        return document.getElementById(id);
    }

    function delay(ms) {
        var sleep = window.unsafeSetTimeout || window.setTimeout;
        return new Promise(function (resolve) {
            sleep(resolve, ms);
        });
    }

    async function waitFor(predicate, timeoutMs, intervalMs) {
        var timeout = timeoutMs || 3000;
        var interval = intervalMs || 25;
        var startedAt = Date.now();

        while (Date.now() - startedAt <= timeout) {
            if (predicate()) {
                return true;
            }
            await delay(interval);
        }

        throw new Error('Timed out after ' + timeout + 'ms');
    }

    function clearResults() {
        var list = byId('helpers-htmx-test-results');
        if (list) {
            list.innerHTML = '';
        }
    }

    function updateSummary(text, statusClass) {
        var summary = byId('helpers-htmx-tests-summary');
        if (!summary) {
            return;
        }

        summary.className = 'badge align-self-center ' + statusClass;
        summary.textContent = text;
    }

    function appendResult(name, passed, details) {
        var list = byId('helpers-htmx-test-results');
        if (!list) {
            return;
        }

        var item = document.createElement('li');
        item.className =
            'list-group-item d-flex justify-content-between align-items-start gap-2';

        var left = document.createElement('span');
        left.textContent = name;

        var right = document.createElement('span');
        right.className = passed ? 'badge bg-success' : 'badge bg-danger';
        right.textContent = passed ? 'PASS' : 'FAIL';

        item.appendChild(left);
        item.appendChild(right);

        if (details) {
            var detail = document.createElement('div');
            detail.className = 'small text-muted mt-1';
            detail.textContent = details;
            item.appendChild(detail);
        }

        list.appendChild(item);
    }

    function assert(condition, message) {
        if (!condition) {
            throw new Error(message);
        }
    }

    async function testBackHelpers() {
        var jsBack = byId('helpers-back-button');
        var dataBack = byId('helpers-data-back-button');
        assert(jsBack && dataBack, 'Back helper fixtures are missing');

        var originalBack = window.history.back.bind(window.history);
        var calls = 0;
        window.history.back = function () {
            calls += 1;
        };

        try {
            jsBack.disabled = false;
            dataBack.disabled = false;
            jsBack.classList.remove('disabled');
            dataBack.classList.remove('disabled');

            jsBack.click();
            dataBack.click();
            await delay(0);

            assert(calls === 2, 'history.back should be called for both fixtures');
            assert(jsBack.disabled && dataBack.disabled, 'Back buttons should be disabled');
            assert(
                jsBack.classList.contains('disabled') &&
                    dataBack.classList.contains('disabled'),
                'Back buttons should receive disabled class'
            );
            return 'history.back calls=' + calls;
        } finally {
            window.history.back = originalBack;
        }
    }

    async function testToggleHelper() {
        var control = byId('helpers-toggle-control');
        var target = byId('helpers-toggle-target');
        assert(control && target, 'Toggle fixtures are missing');

        control.checked = false;
        control.dispatchEvent(new Event('change', { bubbles: true }));
        await delay(0);
        assert(target.disabled === true, 'Target should be disabled when control is unchecked');

        control.checked = true;
        control.dispatchEvent(new Event('change', { bubbles: true }));
        await delay(0);
        assert(target.disabled === false, 'Target should be enabled when control is checked');

        control.checked = false;
        control.dispatchEvent(new Event('change', { bubbles: true }));
        await delay(0);
        assert(target.disabled === true, 'Target should disable again when control is unchecked');

        return 'checked/unchecked transitions verified';
    }

    async function testDateAndTimeFormatting() {
        var ids = [
            'helpers-timeago',
            'helpers-datetime',
            'helpers-date',
            'helpers-time',
        ];

        await waitFor(function () {
            return ids.every(function (id) {
                var node = byId(id);
                return node && node.textContent.trim().length > 0;
            });
        }, 5000);

        return 'all date/time targets populated';
    }

    async function testFlatpickrInitial() {
        var dateInput = byId('helpers-flatpickr-date');
        var datetimeInput = byId('helpers-flatpickr-datetime');
        assert(dateInput && datetimeInput, 'Flatpickr initial fixtures are missing');

        await waitFor(function () {
            return Boolean(dateInput._flatpickr) && Boolean(datetimeInput._flatpickr);
        }, 5000);

        return 'initial date and datetime inputs initialized';
    }

    async function testFilePreview() {
        var input = byId('helpers-file-input');
        var image = byId('helpers-preview-image');
        assert(input && image, 'File preview fixtures are missing');
        assert(typeof DataTransfer !== 'undefined', 'DataTransfer is unavailable');

        var file = new File(
            [new Uint8Array([137, 80, 78, 71, 13, 10, 26, 10])],
            'tiny.png',
            { type: 'image/png' }
        );
        var transfer = new DataTransfer();
        transfer.items.add(file);
        input.files = transfer.files;

        input.dispatchEvent(new Event('change', { bubbles: true }));
        await waitFor(function () {
            var src = image.getAttribute('src') || '';
            return src.indexOf('data:') === 0;
        }, 5000);

        return 'preview src set to data URL';
    }

    async function testFlatpickrAfterSwap() {
        var trigger = byId('helpers-flatpickr-swap-trigger');
        assert(trigger, 'Flatpickr swap trigger is missing');
        trigger.click();

        await waitFor(function () {
            return byId('helpers-flatpickr-swapped-date');
        }, 5000);

        var swappedDate = byId('helpers-flatpickr-swapped-date');
        var swappedDatetime = byId('helpers-flatpickr-swapped-datetime');
        assert(swappedDate && swappedDatetime, 'Swapped flatpickr fields are missing');

        await waitFor(function () {
            return Boolean(swappedDate._flatpickr) && Boolean(swappedDatetime._flatpickr);
        }, 5000);

        return 'swapped date and datetime inputs initialized';
    }

    async function testIhpCompatibilityEvents() {
        var trigger = byId('helpers-event-swap-trigger');
        assert(trigger, 'Event swap trigger is missing');

        var loadCount = 0;
        var unloadCount = 0;

        function onLoad() {
            loadCount += 1;
        }
        function onUnload() {
            unloadCount += 1;
        }

        document.addEventListener('ihp:load', onLoad);
        document.addEventListener('ihp:unload', onUnload);

        try {
            trigger.click();
            await waitFor(function () {
                var marker = byId('helpers-event-marker');
                return marker && marker.dataset.swap === 'done';
            }, 5000);
            await delay(50);

            assert(unloadCount >= 1, 'Expected at least one ihp:unload event');
            assert(loadCount >= 1, 'Expected at least one ihp:load event');
            return 'ihp:load=' + loadCount + ', ihp:unload=' + unloadCount;
        } finally {
            document.removeEventListener('ihp:load', onLoad);
            document.removeEventListener('ihp:unload', onUnload);
        }
    }

    async function testScrollIntoViewAfterSwap() {
        var trigger = byId('helpers-scroll-swap-trigger');
        assert(trigger, 'Scroll swap trigger is missing');

        var original = Element.prototype.scrollIntoView;
        var calledIds = [];

        Element.prototype.scrollIntoView = function () {
            if (this && this.id) {
                calledIds.push(this.id);
            }
        };

        try {
            trigger.click();
            await waitFor(function () {
                return byId('helpers-scroll-target');
            }, 5000);
            await waitFor(function () {
                return calledIds.indexOf('helpers-scroll-target') >= 0;
            }, 3000);
            return 'scrollIntoView called for helpers-scroll-target';
        } finally {
            Element.prototype.scrollIntoView = original;
        }
    }

    async function testAlertDismissOnRequest() {
        var alert = byId('helpers-alert');
        var submit = byId('helpers-alert-submit');
        var result = byId('helpers-alert-result');
        assert(alert && submit && result, 'Alert fixtures are missing');

        alert.classList.remove('dismiss');
        submit.disabled = false;

        submit.click();

        await waitFor(function () {
            return alert.classList.contains('dismiss');
        }, 3000);

        var disabledDuringRequest = false;
        try {
            await waitFor(function () {
                return submit.disabled === true;
            }, 1000, 15);
            disabledDuringRequest = true;
        } catch (_err) {
            disabledDuringRequest = false;
        }

        await waitFor(function () {
            return (result.textContent || '').indexOf('submitted') >= 0;
        }, 5000);

        return 'alert dismissed; hx-disabled-elt observed=' + disabledDuringRequest;
    }

    async function testConfirmOnRequest() {
        var button = byId('helpers-delete-button');
        var result = byId('helpers-delete-result');
        assert(button && result, 'Delete fixtures are missing');

        var originalConfirm = window.confirm;
        var confirmCalls = 0;
        window.confirm = function () {
            confirmCalls += 1;
            return true;
        };

        try {
            button.click();
            await waitFor(function () {
                return (result.textContent || '').indexOf('delete-ok') >= 0;
            }, 5000);
            assert(confirmCalls === 1, 'Expected one confirmation prompt');
            return 'confirm calls=' + confirmCalls;
        } finally {
            window.confirm = originalConfirm;
        }
    }

    async function testMorphdomSwapParity() {
        var swapTrigger = byId('helpers-morphdom-swap-trigger');
        var input = byId('helpers-morph-input');
        var checkbox = byId('helpers-morph-check');
        var select = byId('helpers-morph-select');
        var keyedNode = byId('helpers-node-keyed');

        assert(
            swapTrigger && input && checkbox && select && keyedNode,
            'Morphdom fixtures are missing'
        );

        window.__helpersNodeRef = keyedNode;
        input.value = 'typed-value';
        checkbox.checked = true;
        select.value = 'b';

        swapTrigger.click();

        await waitFor(function () {
            var version = byId('helpers-morph-version');
            return version && version.dataset.version === 'two';
        }, 5000);

        assert(byId('helpers-morph-input').value === 'typed-value', 'Input value should be preserved');
        assert(byId('helpers-morph-check').checked === true, 'Checkbox state should be preserved');
        assert(byId('helpers-morph-select').value === 'b', 'Select value should be preserved');
        assert(
            window.__helpersNodeRef === byId('helpers-node-keyed'),
            'Keyed node identity should be preserved'
        );

        var dynamicButton = byId('helpers-dynamic-hx-button');
        assert(dynamicButton, 'Dynamic hx-get button missing after morphdom swap');
        await waitFor(function () {
            var button = byId('helpers-dynamic-hx-button');
            return Boolean(button && button['htmx-internal-data']);
        }, 5000);
        dynamicButton.click();

        await waitFor(function () {
            var process = byId('helpers-process-result');
            return process && (process.textContent || '').indexOf('pong') >= 0;
        }, 5000);

        return 'input/checkbox/select/node identity and dynamic hx-get verified';
    }

    async function testTimerTrackingUtilities() {
        assert(Array.isArray(window.allIntervals), 'window.allIntervals should exist');
        assert(Array.isArray(window.allTimeouts), 'window.allTimeouts should exist');
        assert(
            typeof window.clearAllIntervals === 'function' &&
                typeof window.clearAllTimeouts === 'function',
            'clearAllIntervals/clearAllTimeouts should exist'
        );

        var intervalsBefore = window.allIntervals.length;
        var timeoutsBefore = window.allTimeouts.length;
        var intervalId = window.setInterval(function () {}, 10000);
        var timeoutId = window.setTimeout(function () {}, 10000);

        var intervalTracked =
            window.allIntervals.length >= intervalsBefore + 1 &&
            window.allIntervals.indexOf(intervalId) >= 0;
        var timeoutTracked =
            window.allTimeouts.length >= timeoutsBefore + 1 &&
            window.allTimeouts.indexOf(timeoutId) >= 0;

        window.clearInterval(intervalId);
        window.clearTimeout(timeoutId);

        assert(intervalTracked, 'setInterval should be tracked in allIntervals');
        assert(timeoutTracked, 'setTimeout should be tracked in allTimeouts');
        return 'intervals/timeouts are tracked by helper wrappers';
    }

    var tests = [
        { name: 'js-back and data-js-back', run: testBackHelpers },
        { name: 'data-toggle behavior', run: testToggleHelper },
        { name: 'date/time formatting helpers', run: testDateAndTimeFormatting },
        { name: 'flatpickr initial init', run: testFlatpickrInitial },
        { name: 'file preview helper', run: testFilePreview },
        { name: 'flatpickr init after HTMX swap', run: testFlatpickrAfterSwap },
        { name: 'ihp:load and ihp:unload events', run: testIhpCompatibilityEvents },
        { name: 'js-scroll-into-view after swap', run: testScrollIntoViewAfterSwap },
        { name: 'dismiss alert + hx-disabled-elt', run: testAlertDismissOnRequest },
        { name: 'hx-confirm on request', run: testConfirmOnRequest },
        { name: 'morphdom parity + htmx.process', run: testMorphdomSwapParity },
        { name: 'timer tracking utilities', run: testTimerTrackingUtilities },
    ];

    var running = false;

    async function runChecks() {
        if (running) {
            return;
        }

        running = true;
        clearResults();
        updateSummary('Running...', 'bg-warning text-dark');

        var passed = 0;
        for (var i = 0; i < tests.length; i += 1) {
            var test = tests[i];
            try {
                var details = await test.run();
                appendResult(test.name, true, details);
                passed += 1;
            } catch (error) {
                appendResult(test.name, false, String(error));
            }
        }

        var success = passed === tests.length;
        updateSummary(
            passed + '/' + tests.length + ' passed',
            success ? 'bg-success' : 'bg-danger'
        );
        running = false;
    }

    function bindRunnerButton() {
        var button = byId('run-helpers-htmx-tests');
        if (!button || button.__helpersHtmxSpecBound) {
            return;
        }

        button.__helpersHtmxSpecBound = true;
        button.addEventListener('click', function () {
            runChecks();
        });
    }

    bindRunnerButton();
    document.addEventListener('ihp:load', bindRunnerButton);
})();
