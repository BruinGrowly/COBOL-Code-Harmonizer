"""
Console Reporter

Beautiful terminal output using Rich library.
"""

from typing import List, Dict
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
from rich.text import Text
from rich import box
from rich.progress import Progress, SpinnerColumn, TextColumn


class ConsoleReporter:
    """Generate beautiful console output for analysis results"""

    def __init__(self):
        self.console = Console()

    def print_header(self):
        """Print application header"""
        header = Text()
        header.append("COBOL Code Harmonizer ", style="bold cyan")
        header.append("💛⚓\n", style="yellow")
        header.append("Semantic Analysis using the LJPW Framework", style="dim")

        self.console.print(Panel(header, border_style="cyan"))

    def print_status(self, message: str):
        """Print status message"""
        self.console.print(f"[blue]ℹ[/blue] {message}")

    def print_success(self, message: str):
        """Print success message"""
        self.console.print(f"[green]✓[/green] {message}")

    def print_warning(self, message: str):
        """Print warning message"""
        self.console.print(f"[yellow]⚠[/yellow] {message}")

    def print_error(self, message: str):
        """Print error message"""
        self.console.print(f"[red]✗[/red] {message}", style="bold red")

    def print_separator(self):
        """Print separator line"""
        self.console.print()

    def print_analysis_results(
        self,
        results: List[Dict],
        threshold: float = 0.5,
        show_all: bool = False,
        verbose: bool = False
    ):
        """
        Print analysis results in beautiful format.

        Args:
            results: List of analysis result dictionaries
            threshold: Minimum disharmony score to report
            show_all: Show all procedures, even harmonious ones
            verbose: Show detailed information
        """
        # Filter results based on threshold
        if not show_all:
            results = [r for r in results if r['disharmony_score'] >= threshold]

        if not results:
            self.print_success(
                f"All procedures are harmonious (disharmony < {threshold})! 🎉"
            )
            return

        # Sort by disharmony score (descending)
        results = sorted(results, key=lambda x: x['disharmony_score'], reverse=True)

        for result in results:
            self._print_procedure_result(result, verbose)

    def _print_procedure_result(self, result: Dict, verbose: bool):
        """Print a single procedure analysis result"""

        # Severity styling
        severity_styles = {
            'harmonious': ('green', '✓', 'HARMONIOUS'),
            'minor_drift': ('yellow', '⚠️', 'MINOR DRIFT'),
            'concerning': ('yellow', '⚠️', 'CONCERNING'),
            'significant': ('red', '🔴', 'SIGNIFICANT'),
            'critical': ('red', '💥', 'CRITICAL')
        }

        color, emoji, label = severity_styles.get(
            result['severity_level'],
            ('white', '?', 'UNKNOWN')
        )

        # Create title
        title = Text()
        title.append(result['procedure_name'], style=f"bold {color}")
        title.append(f"  {emoji} {label}", style=color)

        # Create content
        content = []

        # Disharmony score
        score_color = 'green' if result['disharmony_score'] < 0.3 else \
                     'yellow' if result['disharmony_score'] < 0.8 else 'red'
        content.append(
            f"[{score_color}]Disharmony Score: {result['disharmony_score']:.3f}[/{score_color}]"
        )

        # Dimensional shift
        shift = result['dominant_shift']
        if shift['from'] != shift['to']:
            content.append(
                f"\n[dim]Semantic Shift:[/dim] "
                f"[cyan]{shift['from']}[/cyan] → [magenta]{shift['to']}[/magenta]"
            )

        # Explanation
        content.append(f"\n{result['explanation']}")

        # Verbose info
        if verbose:
            content.append("\n[dim]Intent Coordinates:[/dim]")
            intent = result['intent_coords']
            content.append(
                f"  L={intent['love']:.2f}, "
                f"J={intent['justice']:.2f}, "
                f"P={intent['power']:.2f}, "
                f"W={intent['wisdom']:.2f}"
            )

            content.append("\n[dim]Execution Coordinates:[/dim]")
            execution = result['execution_coords']
            content.append(
                f"  L={execution['love']:.2f}, "
                f"J={execution['justice']:.2f}, "
                f"P={execution['power']:.2f}, "
                f"W={execution['wisdom']:.2f}"
            )

            content.append("\n[dim]Trajectory (Intent → Execution):[/dim]")
            trajectory = result['trajectory']
            for dim in ['love', 'justice', 'power', 'wisdom']:
                delta = trajectory[f'{dim}_delta']
                arrow = '↑' if delta > 0 else '↓' if delta < 0 else '→'
                content.append(f"  {dim.capitalize()}: {arrow} {delta:+.2f}")

        # Suggestions
        if 'suggestions' in result and result['suggestions']:
            content.append("\n[bold cyan]Suggested Names:[/bold cyan]")
            for name, score in result['suggestions'][:3]:
                match_pct = int(score * 100)
                content.append(f"  • {name} ({match_pct}% match)")

        # Create panel
        panel_content = "\n".join(content)
        panel = Panel(
            panel_content,
            title=title,
            border_style=color,
            box=box.ROUNDED
        )

        self.console.print(panel)
        self.console.print()

    def print_summary(self, results: List[Dict], threshold: float):
        """Print summary statistics"""

        total = len(results)
        harmonious = sum(1 for r in results if r['is_harmonious'])
        requires_action = sum(1 for r in results if r['requires_action'])
        above_threshold = sum(1 for r in results if r['disharmony_score'] >= threshold)

        # Count by severity
        severity_counts = {}
        for result in results:
            level = result['severity_level']
            severity_counts[level] = severity_counts.get(level, 0) + 1

        # Create summary table
        table = Table(title="Analysis Summary", box=box.ROUNDED, show_header=False)
        table.add_column("Metric", style="cyan")
        table.add_column("Value", style="bold")

        table.add_row("Total Procedures", str(total))
        table.add_row("Harmonious", f"[green]{harmonious}[/green]")
        table.add_row("Above Threshold", f"[yellow]{above_threshold}[/yellow]")
        table.add_row("Requires Action", f"[red]{requires_action}[/red]")

        # Add severity breakdown
        if severity_counts:
            table.add_row("", "")  # Spacer
            for level, count in sorted(severity_counts.items()):
                label = level.replace('_', ' ').title()
                color = 'green' if level == 'harmonious' else \
                       'yellow' if level in ['minor_drift', 'concerning'] else 'red'
                table.add_row(label, f"[{color}]{count}[/{color}]")

        self.console.print(table)

        # Print overall assessment
        self.console.print()
        if requires_action > 0:
            self.console.print(
                f"[bold red]⚠️  {requires_action} procedure(s) require immediate attention![/bold red]"
            )
        elif above_threshold > 0:
            self.console.print(
                f"[bold yellow]⚠️  {above_threshold} procedure(s) have elevated disharmony.[/bold yellow]"
            )
        else:
            self.console.print(
                "[bold green]✓ All procedures are semantically harmonious! 🎉[/bold green]"
            )

    def print_trajectory_map(self, result: Dict):
        """
        Print semantic trajectory visualization.

        Shows how procedure moves through LJPW space.
        """
        trajectory = result['trajectory']

        table = Table(
            title=f"Semantic Trajectory: {result['procedure_name']}",
            box=box.ROUNDED
        )

        table.add_column("Dimension", style="cyan")
        table.add_column("Intent", justify="right", style="blue")
        table.add_column("Execution", justify="right", style="magenta")
        table.add_column("Δ", justify="right")
        table.add_column("", justify="center")

        intent = result['intent_coords']
        execution = result['execution_coords']

        for dim in ['love', 'justice', 'power', 'wisdom']:
            delta = trajectory[f'{dim}_delta']
            arrow = '↑' if delta > 0.05 else '↓' if delta < -0.05 else '→'
            symbol = '⚠️' if abs(delta) > 0.3 else '✓' if abs(delta) < 0.1 else ''

            table.add_row(
                dim.capitalize(),
                f"{intent[dim]:.2f}",
                f"{execution[dim]:.2f}",
                f"{delta:+.2f}",
                f"{arrow} {symbol}"
            )

        self.console.print(table)

    def print_verb_distribution(self, distribution: Dict):
        """Print verb distribution chart"""

        table = Table(title="Verb Distribution", box=box.SIMPLE)
        table.add_column("Dimension", style="cyan")
        table.add_column("Count", justify="right")
        table.add_column("Bar", justify="left")

        total = sum(distribution.values())
        if total == 0:
            return

        for dimension, count in sorted(distribution.items()):
            percentage = (count / total) * 100
            bar_length = int(percentage / 2)  # Scale to fit
            bar = "█" * bar_length

            color = {
                'Love': 'yellow',
                'Justice': 'blue',
                'Power': 'red',
                'Wisdom': 'green'
            }.get(dimension, 'white')

            table.add_row(
                dimension,
                str(count),
                f"[{color}]{bar}[/{color}] {percentage:.0f}%"
            )

        self.console.print(table)
