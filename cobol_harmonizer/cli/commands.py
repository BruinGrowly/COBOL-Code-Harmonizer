"""
CLI Commands for COBOL Code Harmonizer

Command-line interface using Click framework.
"""

import click
from pathlib import Path
from typing import Optional

from cobol_harmonizer.parser.cobol_parser import COBOLParser
from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator
from cobol_harmonizer.reporter.console_reporter import ConsoleReporter


@click.group()
@click.version_option(version='0.1.0')
def cli():
    """
    COBOL Code Harmonizer - Semantic analysis for COBOL 💛⚓

    Detects when COBOL procedure names contradict their implementations
    using the LJPW (Love, Justice, Power, Wisdom) framework.
    """
    pass


@cli.command()
@click.argument('file_path', type=click.Path(exists=True))
@click.option('--threshold', '-t', type=float, default=0.5,
              help='Disharmony threshold for reporting (default: 0.5)')
@click.option('--show-all', '-a', is_flag=True,
              help='Show all procedures, even harmonious ones')
@click.option('--suggest-names', '-s', is_flag=True,
              help='Suggest better procedure names')
@click.option('--verbose', '-v', is_flag=True,
              help='Show detailed analysis')
def analyze(file_path: str, threshold: float, show_all: bool,
            suggest_names: bool, verbose: bool):
    """
    Analyze a COBOL source file for semantic disharmony.

    Examples:

        cobol-harmonizer analyze myprogram.cbl

        cobol-harmonizer analyze myprogram.cbl --threshold 0.3 --suggest-names

        cobol-harmonizer analyze myprogram.cbl -a -v
    """
    reporter = ConsoleReporter()
    reporter.print_header()

    try:
        # Parse COBOL file
        reporter.print_status(f"Parsing {file_path}...")
        parser = COBOLParser()
        program = parser.parse_file(file_path)

        if not program.procedures:
            reporter.print_warning("No procedures found in PROCEDURE DIVISION")
            return

        reporter.print_success(
            f"Found {len(program.procedures)} procedures in program {program.program_id}"
        )

        # Analyze each procedure
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        results = []

        for procedure in program.procedures:
            # Extract intent and execution
            intent = intent_extractor.extract_intent(procedure.name)
            execution = execution_analyzer.analyze_procedure(procedure)

            # Calculate disharmony
            analysis = calculator.calculate_detailed_analysis(
                procedure.name,
                intent,
                execution
            )

            # Add suggestions if requested
            if suggest_names and not analysis['is_harmonious']:
                suggestions = intent_extractor.suggest_better_names(
                    execution,
                    procedure.name
                )
                analysis['suggestions'] = suggestions

            results.append(analysis)

        # Report results
        reporter.print_separator()
        reporter.print_analysis_results(
            results,
            threshold=threshold,
            show_all=show_all,
            verbose=verbose
        )

        # Print summary
        reporter.print_separator()
        reporter.print_summary(results, threshold)

    except Exception as e:
        reporter.print_error(f"Error analyzing file: {str(e)}")
        if verbose:
            import traceback
            traceback.print_exc()
        raise click.Abort()


@cli.command()
@click.argument('file_path', type=click.Path(exists=True))
@click.option('--output', '-o', type=click.Path(),
              help='Output file path (default: stdout)')
@click.option('--format', '-f', type=click.Choice(['json', 'text', 'markdown']),
              default='text', help='Output format')
def report(file_path: str, output: Optional[str], format: str):
    """
    Generate a detailed report for a COBOL source file.

    Examples:

        cobol-harmonizer report myprogram.cbl

        cobol-harmonizer report myprogram.cbl -o report.json -f json

        cobol-harmonizer report myprogram.cbl -o report.md -f markdown
    """
    reporter = ConsoleReporter()

    try:
        # Parse and analyze
        parser = COBOLParser()
        program = parser.parse_file(file_path)

        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        results = []
        for procedure in program.procedures:
            intent = intent_extractor.extract_intent(procedure.name)
            execution = execution_analyzer.analyze_procedure(procedure)
            analysis = calculator.calculate_detailed_analysis(
                procedure.name,
                intent,
                execution
            )
            results.append(analysis)

        # Generate report based on format
        if format == 'json':
            import json
            report_data = {
                'program_id': program.program_id,
                'file_path': file_path,
                'procedures_analyzed': len(results),
                'results': results
            }
            report_text = json.dumps(report_data, indent=2)

        elif format == 'markdown':
            report_text = _generate_markdown_report(program, results)

        else:  # text
            report_text = _generate_text_report(program, results)

        # Output report
        if output:
            with open(output, 'w') as f:
                f.write(report_text)
            reporter.print_success(f"Report saved to {output}")
        else:
            click.echo(report_text)

    except Exception as e:
        reporter.print_error(f"Error generating report: {str(e)}")
        raise click.Abort()


@cli.command()
def version():
    """Show version information."""
    reporter = ConsoleReporter()
    reporter.print_header()
    click.echo("\nCOBOL Code Harmonizer v0.1.0")
    click.echo("Built on the LJPW semantic framework")
    click.echo("License: MIT")
    click.echo("\nSister projects:")
    click.echo("  - Python Code Harmonizer")
    click.echo("  - JavaScript Code Harmonizer")


@cli.command()
def examples():
    """Show usage examples."""
    examples_text = """
COBOL Code Harmonizer - Usage Examples

Basic Analysis:
  cobol-harmonizer analyze myprogram.cbl

Show All Procedures (including harmonious):
  cobol-harmonizer analyze myprogram.cbl --show-all

Lower Threshold (more sensitive):
  cobol-harmonizer analyze myprogram.cbl --threshold 0.3

Suggest Better Names:
  cobol-harmonizer analyze myprogram.cbl --suggest-names

Verbose Output:
  cobol-harmonizer analyze myprogram.cbl --verbose

Generate JSON Report:
  cobol-harmonizer report myprogram.cbl -o report.json -f json

Generate Markdown Report:
  cobol-harmonizer report myprogram.cbl -o report.md -f markdown

Analyze Example Programs:
  cobol-harmonizer analyze examples/harmonious_example.cbl
  cobol-harmonizer analyze examples/disharmonious_example.cbl

Get Help:
  cobol-harmonizer --help
  cobol-harmonizer analyze --help
"""
    click.echo(examples_text)


def _generate_text_report(program, results):
    """Generate plain text report"""
    lines = []
    lines.append("=" * 70)
    lines.append(f"COBOL Code Harmonizer Report")
    lines.append("=" * 70)
    lines.append(f"\nProgram ID: {program.program_id}")
    lines.append(f"Procedures Analyzed: {len(results)}")
    lines.append("\n" + "-" * 70)

    for result in results:
        lines.append(f"\nProcedure: {result['procedure_name']}")
        lines.append(f"  Disharmony Score: {result['disharmony_score']}")
        lines.append(f"  Severity: {result['severity_level']}")
        lines.append(f"  Intent: {result['intent_coords']}")
        lines.append(f"  Execution: {result['execution_coords']}")
        if result['requires_action']:
            lines.append(f"  ⚠️  ACTION REQUIRED")

    lines.append("\n" + "=" * 70)
    return "\n".join(lines)


def _generate_markdown_report(program, results):
    """Generate Markdown report"""
    lines = []
    lines.append(f"# COBOL Code Harmonizer Report\n")
    lines.append(f"**Program ID:** {program.program_id}\n")
    lines.append(f"**Procedures Analyzed:** {len(results)}\n")
    lines.append("---\n")

    for result in results:
        severity_emoji = {
            'harmonious': '✓',
            'minor_drift': '⚠️',
            'concerning': '⚠️',
            'significant': '🔴',
            'critical': '💥'
        }
        emoji = severity_emoji.get(result['severity_level'], '?')

        lines.append(f"## {emoji} {result['procedure_name']}\n")
        lines.append(f"- **Disharmony Score:** {result['disharmony_score']}")
        lines.append(f"- **Severity:** {result['severity_level']}")
        lines.append(f"- **Dominant Shift:** {result['dominant_shift']['from']} → {result['dominant_shift']['to']}")
        lines.append(f"\n{result['explanation']}\n")
        lines.append("---\n")

    return "\n".join(lines)


if __name__ == '__main__':
    cli()
