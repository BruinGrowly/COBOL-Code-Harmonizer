# Changelog

All notable changes to the COBOL Code Harmonizer extension will be documented in this file.

## [0.1.0] - 2025-11-08

### Added
- Initial MVP release
- Real-time semantic analysis on file save
- Inline diagnostics with color-coded severity levels
- Three analysis commands:
  - Analyze Current File
  - Analyze Workspace
  - Clear Diagnostics
- Configuration settings:
  - Python path customization
  - Harmonizer path auto-detection
  - Disharmony threshold control
  - Analyze-on-save toggle
  - Copybook path configuration
- Context menu integration for COBOL files
- Support for IBM z/OS USS environments
- Diagnostic hover tooltips with LJPW coordinates
- Workspace-wide batch analysis with progress indicator

### Features
- **Severity Levels**: Error (≥1.2), Warning (0.8-1.2), Info (0.5-0.8)
- **Auto-detection**: Finds cobol_harmonizer.py in common locations
- **Copybook Support**: Configurable copybook search paths
- **Performance**: Handles large COBOL files (2000+ LOC)

### Known Limitations
- Analysis runs on save, not as-you-type (LSP coming in v0.2.0)
- Requires manual Python/Harmonizer path configuration in some environments
- No quick-fix actions yet (planned for v0.2.0)

### Technical Details
- Built with VS Code Extension API 1.60.0+
- Integrates with COBOL Code Harmonizer CLI v0.5.0
- Uses JSON output format for analysis results
- Supports COBOL file extensions: .cbl, .cob, .cobol

---

## Upcoming Releases

### [0.2.0] - Planned
- Language Server Protocol (LSP) implementation
- Real-time analysis (as-you-type)
- Quick fixes (auto-rename suggestions)
- Code actions (procedure name suggestions)
- Inline code lens (show disharmony score above procedures)

### [0.3.0] - Planned
- Historical trend tracking
- Team dashboard integration
- Git diff analysis (show new disharmonies)
- Baseline comparison (compare to last commit)

### [0.4.0] - Planned
- SQL/CICS visualization
- LJPW coordinate graphs
- Procedure dependency graphs
- Compliance reporting integration

---

**Note**: This extension is in active development. Feedback and contributions welcome at https://github.com/BruinGrowly/COBOL-Code-Harmonizer
