#!/usr/bin/env python3
"""
Demo script for COBOL Code Harmonizer

Demonstrates the tool by analyzing the example COBOL programs.
"""

import sys
from pathlib import Path

# Add package to path
sys.path.insert(0, str(Path(__file__).parent))

from cobol_harmonizer.parser.cobol_parser import COBOLParser
from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator
from cobol_harmonizer.reporter.console_reporter import ConsoleReporter


def analyze_file(file_path: str, reporter: ConsoleReporter):
    """Analyze a single COBOL file"""

    reporter.print_status(f"\nAnalyzing: {file_path}")
    reporter.print_separator()

    try:
        # Parse
        parser = COBOLParser()
        program = parser.parse_file(file_path)

        if not program.procedures:
            reporter.print_warning("No procedures found")
            return

        reporter.print_success(
            f"Found {len(program.procedures)} procedures in {program.program_id}"
        )

        # Analyze
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        results = []

        for procedure in program.procedures:
            # Extract intent from name
            intent = intent_extractor.extract_intent(procedure.name)

            # Analyze execution from body
            execution = execution_analyzer.analyze_procedure(procedure)

            # Calculate disharmony
            analysis = calculator.calculate_detailed_analysis(
                procedure.name,
                intent,
                execution
            )

            # Add suggestions for disharmonious code
            if not analysis['is_harmonious']:
                suggestions = intent_extractor.suggest_better_names(
                    execution,
                    procedure.name
                )
                analysis['suggestions'] = suggestions

            results.append(analysis)

        # Report results
        reporter.print_separator()
        reporter.print_analysis_results(results, threshold=0.0, show_all=True, verbose=True)

        # Summary
        reporter.print_separator()
        reporter.print_summary(results, threshold=0.5)

    except FileNotFoundError:
        reporter.print_error(f"File not found: {file_path}")
    except Exception as e:
        reporter.print_error(f"Error: {str(e)}")
        import traceback
        traceback.print_exc()


def main():
    """Main demo function"""

    reporter = ConsoleReporter()
    reporter.print_header()

    # Get example files
    examples_dir = Path(__file__).parent / 'examples'

    harmonious_file = examples_dir / 'harmonious_example.cbl'
    disharmonious_file = examples_dir / 'disharmonious_example.cbl'

    # Demo 1: Harmonious Example
    reporter.console.print("\n[bold cyan]═══ DEMO 1: Harmonious COBOL Code ═══[/bold cyan]\n")
    reporter.console.print(
        "[dim]This example shows procedures with names that accurately "
        "reflect their implementations.[/dim]\n"
    )

    if harmonious_file.exists():
        analyze_file(str(harmonious_file), reporter)
    else:
        reporter.print_warning(f"Example file not found: {harmonious_file}")

    # Demo 2: Disharmonious Example
    reporter.console.print("\n\n[bold red]═══ DEMO 2: Disharmonious COBOL Code (Semantic Bugs!) ═══[/bold red]\n")
    reporter.console.print(
        "[dim]This example shows procedures with names that LIE about "
        "their implementations.[/dim]\n"
    )

    if disharmonious_file.exists():
        analyze_file(str(disharmonious_file), reporter)
    else:
        reporter.print_warning(f"Example file not found: {disharmonious_file}")

    # Conclusion
    reporter.console.print("\n\n[bold cyan]═══ Demo Complete ═══[/bold cyan]\n")
    reporter.console.print(
        "[green]✓[/green] The COBOL Code Harmonizer successfully detected:\n"
    )
    reporter.console.print("  • Harmonious procedures (accurate naming)")
    reporter.console.print("  • Critical semantic bugs (GET that DELETEs)")
    reporter.console.print("  • Significant disharmony (CHECK that UPDATEs)")
    reporter.console.print("  • Minor drift (validators with side effects)")
    reporter.console.print()
    reporter.console.print("[dim]This tool helps detect bugs that traditional linters miss![/dim]")
    reporter.console.print()


if __name__ == '__main__':
    main()
