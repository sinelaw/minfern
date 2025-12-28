// Minfern Web App - Live JavaScript Type Checker

import init, { check_types } from './pkg/minfern.js';

// State
let wasm = null;
let inputEditor = null;
let checkTimeout = null;
const DEBOUNCE_MS = 300;

// Example code
const EXAMPLE_CODE = `// Welcome to Minfern!
// A static type checker for JavaScript with HMF-based inference.
// Start typing to see inferred types live.

// Simple function - types are inferred automatically
function add(a, b) {
    return a + b;
}

var sum = add(1, 2);

// Objects with structural typing
var person = {
    name: "Alice",
    age: 30,
    greet: function() {
        return "Hello, " + this.name;
    }
};

var greeting = person.greet();

// Higher-order functions
function map(arr, fn) {
    var result = [];
    for (var i = 0; i < arr.length; i++) {
        result[i] = fn(arr[i]);
    }
    return result;
}

var numbers = [1, 2, 3];
var doubled = map(numbers, function(x) { return x * 2; });

// Try introducing a type error:
// var bad = add("hello", 42);
`;

// DOM elements
const statusEl = document.getElementById('status');
const outputEl = document.getElementById('output-editor');
const programTypeEl = document.getElementById('program-type');
const errorPanel = document.getElementById('error-panel');
const errorContent = document.getElementById('error-content');
const closeErrorsBtn = document.getElementById('close-errors');
const formatBtn = document.getElementById('format-btn');
const divider = document.getElementById('divider');
const inputPane = document.getElementById('input-pane');

// Initialize
async function initialize() {
    try {
        wasm = await init();
        statusEl.textContent = 'Ready';
        statusEl.classList.add('ready');

        // Set up CodeMirror
        inputEditor = CodeMirror.fromTextArea(document.getElementById('input-editor'), {
            mode: 'javascript',
            theme: 'dracula',
            lineNumbers: true,
            matchBrackets: true,
            autoCloseBrackets: true,
            indentUnit: 4,
            tabSize: 4,
            indentWithTabs: false,
        });

        inputEditor.setValue(EXAMPLE_CODE);
        inputEditor.on('change', scheduleCheck);

        // Initial check
        checkTypes();

    } catch (e) {
        console.error('Failed to initialize WASM:', e);
        statusEl.textContent = 'Failed to load';
        statusEl.classList.add('error');
        outputEl.innerHTML = '<span class="empty-state">Failed to load type checker. Make sure the WASM module is built.</span>';
    }
}

// Schedule a type check with debouncing
function scheduleCheck() {
    if (checkTimeout) {
        clearTimeout(checkTimeout);
    }
    checkTimeout = setTimeout(checkTypes, DEBOUNCE_MS);
}

// Perform type checking
function checkTypes() {
    if (!wasm) return;

    const source = inputEditor.getValue();

    if (!source.trim()) {
        outputEl.innerHTML = '<span class="empty-state">Enter some JavaScript code to check types...</span>';
        programTypeEl.textContent = '';
        hideErrors();
        return;
    }

    try {
        const result = check_types(source);

        if (result.success) {
            // Show successful output with syntax highlighting
            outputEl.innerHTML = highlightOutput(result.output, result.program_type);
            programTypeEl.textContent = `Type: ${result.program_type}`;
            programTypeEl.style.color = '';
            hideErrors();
            clearEditorErrors();
        } else {
            // Show errors
            outputEl.innerHTML = '<span class="empty-state">Type checking failed. See errors below.</span>';
            programTypeEl.textContent = '';
            showErrors(result.errors, source);
        }
    } catch (e) {
        console.error('Type check error:', e);
        outputEl.innerHTML = `<span class="empty-state">Error: ${e.message}</span>`;
    }
}

// Syntax highlight the output
function highlightOutput(code, programType) {
    // First, escape HTML
    let escaped = escapeHtml(code);

    // Highlight type comments
    escaped = escaped.replace(
        /\/\*:\s*([^*]+)\s*\*\//g,
        '<span class="type-annotation">/*: $1 */</span>'
    );

    // Highlight regular comments
    escaped = escaped.replace(
        /(\/\/[^\n]*)/g,
        '<span class="comment">$1</span>'
    );

    // Highlight keywords
    const keywords = ['function', 'var', 'let', 'const', 'if', 'else', 'for', 'while', 'do',
                      'return', 'throw', 'try', 'catch', 'finally', 'new', 'typeof', 'instanceof',
                      'in', 'of', 'break', 'continue', 'switch', 'case', 'default', 'this', 'null',
                      'undefined', 'true', 'false', 'delete', 'void'];

    const keywordPattern = new RegExp(`\\b(${keywords.join('|')})\\b`, 'g');
    escaped = escaped.replace(keywordPattern, '<span class="keyword">$1</span>');

    // Highlight strings
    escaped = escaped.replace(
        /("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*')/g,
        '<span class="string">$1</span>'
    );

    // Highlight numbers
    escaped = escaped.replace(
        /\b(\d+\.?\d*(?:e[+-]?\d+)?)\b/gi,
        '<span class="number">$1</span>'
    );

    // Highlight function names (function keyword followed by name)
    escaped = escaped.replace(
        /(<span class="keyword">function<\/span>)\s+(\w+)/g,
        '$1 <span class="function-name">$2</span>'
    );

    // Add program type comment at the top
    const typeComment = `<span class="comment">// Program type: ${escapeHtml(programType)}</span>\n\n`;

    return typeComment + escaped;
}

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Show error panel
function showErrors(errors, source) {
    errorContent.innerHTML = '';

    errors.forEach(error => {
        const item = document.createElement('div');
        item.className = 'error-item';

        // Calculate line and column from offset
        const loc = offsetToLineCol(source, error.start);

        item.innerHTML = `
            <div class="message">${escapeHtml(error.message)}</div>
            <div class="location">Line ${loc.line}, Column ${loc.column}</div>
        `;

        item.addEventListener('click', () => {
            inputEditor.setCursor({ line: loc.line - 1, ch: loc.column - 1 });
            inputEditor.focus();
        });

        errorContent.appendChild(item);
    });

    errorPanel.classList.add('visible');

    // Mark errors in editor
    markEditorErrors(errors, source);
}

// Hide error panel
function hideErrors() {
    errorPanel.classList.remove('visible');
}

// Convert byte offset to line/column
function offsetToLineCol(source, offset) {
    let line = 1;
    let column = 1;

    for (let i = 0; i < offset && i < source.length; i++) {
        if (source[i] === '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
    }

    return { line, column };
}

// Mark errors in the editor
function markEditorErrors(errors, source) {
    clearEditorErrors();

    errors.forEach(error => {
        const startLoc = offsetToLineCol(source, error.start);
        const endLoc = offsetToLineCol(source, error.end);

        inputEditor.markText(
            { line: startLoc.line - 1, ch: startLoc.column - 1 },
            { line: endLoc.line - 1, ch: endLoc.column - 1 },
            { className: 'error-underline' }
        );
    });
}

// Clear error marks from editor
function clearEditorErrors() {
    inputEditor.getAllMarks().forEach(mark => mark.clear());
}

// Divider drag handling
let isDragging = false;

divider.addEventListener('mousedown', (e) => {
    isDragging = true;
    divider.classList.add('dragging');
    document.body.style.cursor = 'col-resize';
    e.preventDefault();
});

document.addEventListener('mousemove', (e) => {
    if (!isDragging) return;

    const containerRect = document.querySelector('main').getBoundingClientRect();
    const percentage = ((e.clientX - containerRect.left) / containerRect.width) * 100;

    if (percentage > 20 && percentage < 80) {
        inputPane.style.flex = `0 0 ${percentage}%`;
    }
});

document.addEventListener('mouseup', () => {
    if (isDragging) {
        isDragging = false;
        divider.classList.remove('dragging');
        document.body.style.cursor = '';
        inputEditor.refresh();
    }
});

// Button handlers
formatBtn.addEventListener('click', checkTypes);
closeErrorsBtn.addEventListener('click', hideErrors);

// Keyboard shortcuts
document.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
        e.preventDefault();
        checkTypes();
    }
});

// Start
initialize();
