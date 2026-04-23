// minfern-checked SPA: a classic todo list without React or any framework.
//
// State lives in a single plain object. Every event handler mutates that
// state and calls `render()`, which rebuilds the list portion of the DOM
// from an HTML string and re-attaches per-item handlers. Every object
// shape and function type in this file is inferred by minfern — there is
// not a single type annotation in the body of the app.
//
// Relies on minfern features:
//   - Embedded stdlib: `document`, `console`, `Math`, ... come from
//     stdlib/dom.d.js and stdlib/core.d.js auto-loaded by the checker.
//   - Function hoisting: `render()` is defined at the bottom, called from
//     handlers near the top, and the checker sees it fine.
//   - Arrow functions, `let`, `const`.
//   - `Array.prototype` and `String.prototype` methods (filter/map/
//     forEach/some/reduce, replaceAll).
//
// To type-check:
//     minfern examples/spa/app.js

// ---------------------------------------------------------------------------
// State. Seeding the todos list with two entries fixes its element type to
// {id: Number, text: String, done: Boolean} without needing annotations.
// ---------------------------------------------------------------------------

let state = {
    todos: [
        {id: 1, text: "Try editing this todo list", done: false},
        {id: 2, text: "Read examples/spa/app.js", done: true}
    ],
    nextId: 3,
    filter: "all"
};

// ---------------------------------------------------------------------------
// Pure helpers.
// ---------------------------------------------------------------------------

function escapeHtml(s) {
    return s
        .replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")
        .replaceAll("\"", "&quot;")
        .replaceAll("'", "&#39;");
}

function countRemaining() {
    return state.todos.filter(t => !t.done).length;
}

function hasAnyDone() {
    return state.todos.some(t => t.done);
}

function shouldShow(todo) {
    if (state.filter === "active") return !todo.done;
    if (state.filter === "done") return todo.done;
    return true;
}

// ---------------------------------------------------------------------------
// State updates.
// ---------------------------------------------------------------------------

function addTodo(text) {
    if (text === "") return;
    state.todos.push({id: state.nextId, text: text, done: false});
    state.nextId = state.nextId + 1;
}

function toggleTodo(id) {
    state.todos.forEach(t => {
        if (t.id === id) t.done = !t.done;
        return undefined;
    });
}

function deleteTodo(id) {
    state.todos = state.todos.filter(t => t.id !== id);
}

function clearDone() {
    state.todos = state.todos.filter(t => !t.done);
}

function setFilter(f) {
    state.filter = f;
}

// ---------------------------------------------------------------------------
// Rendering. The list and footer are rebuilt from scratch on every update.
// ---------------------------------------------------------------------------

function renderTodoItem(todo) {
    let liClass = todo.done ? "done" : "";
    let toggleClass = todo.done ? "toggle on" : "toggle";
    let toggleMark = todo.done ? "✓" : "";
    return (
        `<li class="${liClass}">` +
            `<button id="toggle-${todo.id}" class="${toggleClass}">${toggleMark}</button>` +
            `<span class="text">${escapeHtml(todo.text)}</span>` +
            `<button id="delete-${todo.id}" class="delete" title="Delete">×</button>` +
        `</li>`
    );
}

function renderList() {
    let listEl = document.getElementById("todo-list");
    let visible = state.todos.filter(shouldShow);
    let html = "";
    if (visible.length === 0) {
        html = state.todos.length === 0
            ? `<li class="empty">No todos yet. Add one above.</li>`
            : `<li class="empty">Nothing matches the current filter.</li>`;
    } else {
        html = visible.map(renderTodoItem).join("");
    }
    listEl.innerHTML = html;
}

function renderFooter() {
    let footer = document.getElementById("footer");
    let remaining = countRemaining();
    let word = remaining === 1 ? "item" : "items";
    let clearMarkup = hasAnyDone()
        ? `<button id="clear-done">Clear completed</button>`
        : `<span></span>`;
    footer.innerHTML = `<span>${remaining} ${word} left</span>` + clearMarkup;
}

function renderFilters() {
    ["all", "active", "done"].forEach(name => {
        document.getElementById(`filter-${name}`).className =
            state.filter === name ? "active" : "";
        return undefined;
    });
}

function attachListHandlers() {
    state.todos.forEach(todo => {
        if (shouldShow(todo)) {
            document.getElementById(`toggle-${todo.id}`).onclick = () => {
                toggleTodo(todo.id);
                render();
                return undefined;
            };
            document.getElementById(`delete-${todo.id}`).onclick = () => {
                deleteTodo(todo.id);
                render();
                return undefined;
            };
        }
        return undefined;
    });
    if (hasAnyDone()) {
        document.getElementById("clear-done").onclick = () => {
            clearDone();
            render();
            return undefined;
        };
    }
}

function render() {
    renderList();
    renderFooter();
    renderFilters();
    attachListHandlers();
}

// ---------------------------------------------------------------------------
// Top-level wiring: the three static elements that live for the whole session.
// ---------------------------------------------------------------------------

function submitDraft() {
    let input = document.getElementById("new-input");
    addTodo(input.value);
    input.value = "";
    render();
}

document.getElementById("add-btn").onclick = () => {
    submitDraft();
    return undefined;
};

document.getElementById("new-input").onkeydown = e => {
    if (e.key === "Enter") submitDraft();
    return undefined;
};

["all", "active", "done"].forEach(name => {
    document.getElementById(`filter-${name}`).onclick = () => {
        setFilter(name);
        render();
        return undefined;
    };
    return undefined;
});

render();
