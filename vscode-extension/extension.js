const vscode = require('vscode');
const path = require('path');
const { exec } = require('child_process');
const { promisify } = require('util');
const fs = require('fs').promises;

const execAsync = promisify(exec);

/**
 * @type {vscode.DiagnosticCollection}
 */
let diagnosticCollection;

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    console.log('COBOL Harmonizer extension is now active');

    // Create diagnostic collection
    diagnosticCollection = vscode.languages.createDiagnosticCollection('cobol-harmonizer');
    context.subscriptions.push(diagnosticCollection);

    // Register commands
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolHarmonizer.analyzeFile', analyzeCurrentFile)
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('cobolHarmonizer.analyzeWorkspace', analyzeWorkspace)
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('cobolHarmonizer.clearDiagnostics', () => {
            diagnosticCollection.clear();
            vscode.window.showInformationMessage('COBOL Harmonizer diagnostics cleared');
        })
    );

    // Analyze on save if enabled
    context.subscriptions.push(
        vscode.workspace.onDidSaveTextDocument(async (document) => {
            const config = vscode.workspace.getConfiguration('cobolHarmonizer');
            if (config.get('analyzeOnSave') && document.languageId === 'cobol') {
                await analyzeDocument(document);
            }
        })
    );

    // Show welcome message
    vscode.window.showInformationMessage('COBOL Harmonizer ready! Right-click in a COBOL file to analyze.');
}

/**
 * Analyze the currently active file
 */
async function analyzeCurrentFile() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage('No active editor');
        return;
    }

    if (editor.document.languageId !== 'cobol') {
        vscode.window.showWarningMessage('Current file is not a COBOL file');
        return;
    }

    await analyzeDocument(editor.document);
}

/**
 * Analyze all COBOL files in workspace
 */
async function analyzeWorkspace() {
    const cobolFiles = await vscode.workspace.findFiles('**/*.{cbl,cob,cobol}', '**/node_modules/**');

    if (cobolFiles.length === 0) {
        vscode.window.showInformationMessage('No COBOL files found in workspace');
        return;
    }

    await vscode.window.withProgress({
        location: vscode.ProgressLocation.Notification,
        title: `Analyzing ${cobolFiles.length} COBOL files...`,
        cancellable: false
    }, async (progress) => {
        for (let i = 0; i < cobolFiles.length; i++) {
            const doc = await vscode.workspace.openTextDocument(cobolFiles[i]);
            await analyzeDocument(doc, false);
            progress.report({
                increment: (100 / cobolFiles.length),
                message: `${i + 1}/${cobolFiles.length} files`
            });
        }
    });

    vscode.window.showInformationMessage(`Analyzed ${cobolFiles.length} COBOL files`);
}

/**
 * Analyze a single document
 * @param {vscode.TextDocument} document
 * @param {boolean} showMessage
 */
async function analyzeDocument(document, showMessage = true) {
    const config = vscode.workspace.getConfiguration('cobolHarmonizer');
    const pythonPath = config.get('pythonPath', 'python3');
    const harmonizerPath = await findHarmonizerPath(config.get('harmonizerPath'));
    const threshold = config.get('disharmonyThreshold', 0.5);

    if (!harmonizerPath) {
        vscode.window.showErrorMessage('Could not find cobol_harmonizer.py. Please configure cobolHarmonizer.harmonizerPath');
        return;
    }

    const filePath = document.uri.fsPath;
    const tempOutputPath = path.join(require('os').tmpdir(), `harmonizer-${Date.now()}.json`);

    try {
        // Build command with copybook paths if configured
        const copybookPaths = config.get('copybookPaths', []);
        let copybookArgs = '';
        if (copybookPaths.length > 0) {
            copybookArgs = copybookPaths.map(p => `--copybook-path "${p}"`).join(' ');
        }

        const command = `"${pythonPath}" "${harmonizerPath}" analyze --input "${filePath}" --output "${tempOutputPath}" ${copybookArgs}`;

        if (showMessage) {
            vscode.window.showInformationMessage('Analyzing COBOL file...');
        }

        // Execute analysis
        await execAsync(command, { timeout: 30000 });

        // Read results
        const resultJson = await fs.readFile(tempOutputPath, 'utf8');
        const results = JSON.parse(resultJson);

        // Process results and create diagnostics
        const diagnostics = [];

        if (results.procedures) {
            for (const proc of results.procedures) {
                if (proc.disharmony_score >= threshold) {
                    const diagnostic = createDiagnostic(document, proc);
                    if (diagnostic) {
                        diagnostics.push(diagnostic);
                    }
                }
            }
        }

        // Update diagnostics
        diagnosticCollection.set(document.uri, diagnostics);

        if (showMessage) {
            if (diagnostics.length === 0) {
                vscode.window.showInformationMessage('✓ No semantic disharmony detected!');
            } else {
                vscode.window.showWarningMessage(`Found ${diagnostics.length} procedure(s) with semantic disharmony`);
            }
        }

        // Clean up temp file
        await fs.unlink(tempOutputPath).catch(() => {});

    } catch (error) {
        const errorMsg = error.message || error.toString();
        vscode.window.showErrorMessage(`Analysis failed: ${errorMsg}`);
        console.error('COBOL Harmonizer error:', error);
    }
}

/**
 * Create a diagnostic from a procedure result
 * @param {vscode.TextDocument} document
 * @param {object} proc
 * @returns {vscode.Diagnostic | null}
 */
function createDiagnostic(document, proc) {
    // Find the procedure in the document
    const text = document.getText();
    const procPattern = new RegExp(`\\b${escapeRegex(proc.name)}\\b`, 'i');
    const match = procPattern.exec(text);

    if (!match) {
        return null;
    }

    const startPos = document.positionAt(match.index);
    const endPos = document.positionAt(match.index + proc.name.length);
    const range = new vscode.Range(startPos, endPos);

    const severity = getSeverity(proc.disharmony_score);
    const message = formatDiagnosticMessage(proc);

    const diagnostic = new vscode.Diagnostic(range, message, severity);
    diagnostic.source = 'COBOL Harmonizer';
    diagnostic.code = 'semantic-disharmony';

    return diagnostic;
}

/**
 * Format a diagnostic message
 * @param {object} proc
 * @returns {string}
 */
function formatDiagnosticMessage(proc) {
    const score = proc.disharmony_score.toFixed(2);
    const classification = getClassification(proc.disharmony_score);

    let message = `Semantic Disharmony (${score}) - ${classification}\n`;
    message += `Intent: L=${proc.intent_coords.love.toFixed(2)} `;
    message += `J=${proc.intent_coords.justice.toFixed(2)} `;
    message += `P=${proc.intent_coords.power.toFixed(2)} `;
    message += `W=${proc.intent_coords.wisdom.toFixed(2)}\n`;
    message += `Execution: L=${proc.execution_coords.love.toFixed(2)} `;
    message += `J=${proc.execution_coords.justice.toFixed(2)} `;
    message += `P=${proc.execution_coords.power.toFixed(2)} `;
    message += `W=${proc.execution_coords.wisdom.toFixed(2)}`;

    return message;
}

/**
 * Get severity level based on disharmony score
 * @param {number} score
 * @returns {vscode.DiagnosticSeverity}
 */
function getSeverity(score) {
    if (score >= 1.2) return vscode.DiagnosticSeverity.Error;
    if (score >= 0.8) return vscode.DiagnosticSeverity.Warning;
    return vscode.DiagnosticSeverity.Information;
}

/**
 * Get classification label
 * @param {number} score
 * @returns {string}
 */
function getClassification(score) {
    if (score >= 1.2) return 'CRITICAL - Immediate attention required';
    if (score >= 0.8) return 'SIGNIFICANT - Review recommended';
    if (score >= 0.5) return 'CONCERNING - Consider refactoring';
    if (score >= 0.3) return 'MINOR DRIFT - Monitor';
    return 'HARMONIOUS';
}

/**
 * Find the cobol_harmonizer.py path
 * @param {string} configuredPath
 * @returns {Promise<string | null>}
 */
async function findHarmonizerPath(configuredPath) {
    if (configuredPath) {
        try {
            await fs.access(configuredPath);
            return configuredPath;
        } catch {
            return null;
        }
    }

    // Auto-detect strategies
    const searchPaths = [
        // Workspace root
        path.join(vscode.workspace.workspaceFolders?.[0]?.uri.fsPath || '', 'cobol_harmonizer.py'),
        // Common installation locations
        '/usr/local/bin/cobol_harmonizer.py',
        '/usr/bin/cobol_harmonizer.py',
        // z/OS USS
        '/u/cobol-harmonizer/cobol_harmonizer.py',
        // User home
        path.join(require('os').homedir(), 'cobol-harmonizer', 'cobol_harmonizer.py')
    ];

    for (const searchPath of searchPaths) {
        try {
            await fs.access(searchPath);
            return searchPath;
        } catch {
            continue;
        }
    }

    return null;
}

/**
 * Escape regex special characters
 * @param {string} str
 * @returns {string}
 */
function escapeRegex(str) {
    return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function deactivate() {
    if (diagnosticCollection) {
        diagnosticCollection.dispose();
    }
}

module.exports = {
    activate,
    deactivate
};
