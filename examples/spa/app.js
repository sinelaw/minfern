// minfern-checked SPA: a classic todo list without React or any framework.
//
// State lives in a single plain object. Every event handler mutates that
// state and calls the render function, which rebuilds the list portion of
// the DOM from an HTML string and re-attaches per-item handlers. Every
// object shape and function type in this file is inferred by minfern.
//
// To type-check:
//     minfern examples/spa/app.js

// ---------------------------------------------------------------------------
// DOM types come from minfern's embedded stdlib (stdlib/dom.d.js), which is
// auto-loaded by the type checker. At runtime the browser supplies the real
// `document` global. No local declaration needed.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// State.
//
// Seeding the todos list with two entries fixes its element type to
// {id: Number, text: String, done: Boolean} without needing annotations.
// ---------------------------------------------------------------------------

var state = {
    todos: [
        {id: 1, text: "Try editing this todo list", done: false},
        {id: 2, text: "Read examples/spa/app.js", done: true}
    ],
    nextId: 3,
    filter: "all"
};

// ---------------------------------------------------------------------------
// Forward reference for the top-level render function.
//
// minfern does not hoist function declarations, so the event handlers
// created lower in the file cannot yet see `render`. We declare a mutable
// placeholder here and overwrite it after the real renderer is defined.
// ---------------------------------------------------------------------------

var doRender = function () { return undefined; };

// ---------------------------------------------------------------------------
// Pure helpers.
// ---------------------------------------------------------------------------

function escapeHtml(s) {
    // Concatenating with "" pins `s` to String. Without this, minfern's
    // Indexable type class lets `s` float between String and String[]
    // because both have `.length` and `[Number]` returning String.
    var out = "" + s;
    out = "";
    for (var i = 0; i < s.length; i++) {
        var ch = s[i];
        if (ch === "<") {
            out = out + "&lt;";
        } else if (ch === ">") {
            out = out + "&gt;";
        } else if (ch === "&") {
            out = out + "&amp;";
        } else if (ch === "\"") {
            out = out + "&quot;";
        } else if (ch === "'") {
            out = out + "&#39;";
        } else {
            out = out + ch;
        }
    }
    return out;
}

function countRemaining() {
    var n = 0;
    for (var i = 0; i < state.todos.length; i++) {
        if (!state.todos[i].done) {
            n = n + 1;
        }
    }
    return n;
}

function hasAnyDone() {
    for (var i = 0; i < state.todos.length; i++) {
        if (state.todos[i].done) {
            return true;
        }
    }
    return false;
}

function shouldShow(todo) {
    if (state.filter === "active") {
        return !todo.done;
    }
    if (state.filter === "done") {
        return todo.done;
    }
    return true;
}

// ---------------------------------------------------------------------------
// State updates.
// ---------------------------------------------------------------------------

function addTodo(text) {
    if (text === "") {
        return undefined;
    }
    var newTodo = {id: state.nextId, text: text, done: false};
    state.todos[state.todos.length] = newTodo;
    state.nextId = state.nextId + 1;
    return undefined;
}

function toggleTodo(id) {
    for (var i = 0; i < state.todos.length; i++) {
        if (state.todos[i].id === id) {
            state.todos[i].done = !state.todos[i].done;
        }
    }
}

function deleteTodo(id) {
    var kept = [];
    for (var i = 0; i < state.todos.length; i++) {
        var t = state.todos[i];
        if (t.id !== id) {
            kept[kept.length] = t;
        }
    }
    state.todos = kept;
}

function clearDone() {
    var kept = [];
    for (var i = 0; i < state.todos.length; i++) {
        var t = state.todos[i];
        if (!t.done) {
            kept[kept.length] = t;
        }
    }
    state.todos = kept;
}

function setFilter(f) {
    state.filter = f;
}

// ---------------------------------------------------------------------------
// Handler factories.
//
// Each closes over a specific id or filter name so it keeps a concrete
// type like `(Number) => () => Undefined` rather than unifying with the
// changing loop variable.
// ---------------------------------------------------------------------------

function makeToggleHandler(id) {
    return function () {
        toggleTodo(id);
        doRender();
        return undefined;
    };
}

function makeDeleteHandler(id) {
    return function () {
        deleteTodo(id);
        doRender();
        return undefined;
    };
}

function makeFilterHandler(name) {
    return function () {
        setFilter(name);
        doRender();
        return undefined;
    };
}

function makeClearDoneHandler() {
    return function () {
        clearDone();
        doRender();
        return undefined;
    };
}

// ---------------------------------------------------------------------------
// Rendering. The list and footer are rebuilt from scratch on every update.
// ---------------------------------------------------------------------------

function renderTodoItem(todo) {
    var liClass = todo.done ? "done" : "";
    var toggleClass = todo.done ? "toggle on" : "toggle";
    var toggleMark = todo.done ? "✓" : "";
    return (
        `<li class="${liClass}">` +
            `<button id="toggle-${todo.id}" class="${toggleClass}">${toggleMark}</button>` +
            `<span class="text">${escapeHtml(todo.text)}</span>` +
            `<button id="delete-${todo.id}" class="delete" title="Delete">×</button>` +
        `</li>`
    );
}

function renderList() {
    var listEl = document.getElementById("todo-list");
    var html = "";
    var shown = 0;
    for (var i = 0; i < state.todos.length; i++) {
        var todo = state.todos[i];
        if (shouldShow(todo)) {
            html = html + renderTodoItem(todo);
            shown = shown + 1;
        }
    }
    if (shown === 0) {
        if (state.todos.length === 0) {
            html = `<li class="empty">No todos yet. Add one above.</li>`;
        } else {
            html = `<li class="empty">Nothing matches the current filter.</li>`;
        }
    }
    listEl.innerHTML = html;
}

function renderFooter() {
    var footer = document.getElementById("footer");
    var remaining = countRemaining();
    var word = remaining === 1 ? "item" : "items";
    var clearMarkup = hasAnyDone()
        ? `<button id="clear-done">Clear completed</button>`
        : `<span></span>`;
    footer.innerHTML =
        `<span>${remaining} ${word} left</span>` +
        clearMarkup;
}

function renderFilters() {
    var names = ["all", "active", "done"];
    for (var i = 0; i < names.length; i++) {
        var name = names[i];
        var btn = document.getElementById(`filter-${name}`);
        btn.className = state.filter === name ? "active" : "";
    }
}

function attachListHandlers() {
    for (var i = 0; i < state.todos.length; i++) {
        var todo = state.todos[i];
        if (shouldShow(todo)) {
            var toggleBtn = document.getElementById(`toggle-${todo.id}`);
            toggleBtn.onclick = makeToggleHandler(todo.id);
            var deleteBtn = document.getElementById(`delete-${todo.id}`);
            deleteBtn.onclick = makeDeleteHandler(todo.id);
        }
    }
    if (hasAnyDone()) {
        var clearBtn = document.getElementById("clear-done");
        clearBtn.onclick = makeClearDoneHandler();
    }
}

function renderImpl() {
    renderList();
    renderFooter();
    renderFilters();
    attachListHandlers();
    return undefined;
}

// Swap the placeholder for the real renderer. Handlers captured above still
// read `doRender` fresh on every call, so they pick up the new function.
doRender = renderImpl;

// ---------------------------------------------------------------------------
// Top-level wiring: static elements that exist for the whole session.
// ---------------------------------------------------------------------------

function submitDraft() {
    var input = document.getElementById("new-input");
    addTodo(input.value);
    input.value = "";
    doRender();
}

var addBtn = document.getElementById("add-btn");
addBtn.onclick = function () {
    submitDraft();
    return undefined;
};

var newInput = document.getElementById("new-input");
newInput.onkeydown = function (e) {
    if (e.key === "Enter") {
        submitDraft();
    }
    return undefined;
};

var filterNames = ["all", "active", "done"];
for (var fi = 0; fi < filterNames.length; fi = fi + 1) {
    var fname = filterNames[fi];
    var filterBtn = document.getElementById(`filter-${fname}`);
    filterBtn.onclick = makeFilterHandler(fname);
}

doRender();
