#!/usr/bin/env python3
"""
Demo: Call Graph Analysis

Shows how the call graph system works:
- Extracting CALL and PERFORM statements
- Building the call graph
- Impact analysis
- Visualization
"""

from pathlib import Path

from cobol_harmonizer.callgraph import (
    CallExtractor,
    CallGraphBuilder,
    CallGraphAnalyzer,
)
from cobol_harmonizer.callgraph.visualizer import CallGraphVisualizer
from cobol_harmonizer.parser import COBOLParser


def create_sample_program():
    """Create a sample COBOL program for demo"""
    program = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. CUSTOMER-PROCESSOR.

      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01  WS-COUNTER              PIC 9(6) VALUE ZERO.
      01  WS-TOTAL                PIC 9(8) VALUE ZERO.
      01  WS-SUBPROG-NAME         PIC X(8) VALUE 'CALCUTIL'.

      PROCEDURE DIVISION.
      MAIN-PROCEDURE.
          DISPLAY 'Starting customer processing...'.
          PERFORM INITIALIZE-DATA.
          PERFORM PROCESS-CUSTOMERS.
          PERFORM GENERATE-REPORT.
          STOP RUN.

      INITIALIZE-DATA.
          DISPLAY 'Initializing data...'.
          MOVE ZERO TO WS-COUNTER.
          MOVE ZERO TO WS-TOTAL.
          CALL 'LOGGER' USING 'INFO' 'System initialized'.

      PROCESS-CUSTOMERS.
          DISPLAY 'Processing customers...'.
          PERFORM READ-CUSTOMER-FILE.
          PERFORM VALIDATE-CUSTOMER.
          PERFORM UPDATE-DATABASE.

      READ-CUSTOMER-FILE.
          DISPLAY 'Reading customer file...'.
          ADD 1 TO WS-COUNTER.
          CALL 'FILEIO' USING 'READ' 'CUSTFILE'.

      VALIDATE-CUSTOMER.
          DISPLAY 'Validating customer...'.
          PERFORM CHECK-BALANCE.
          PERFORM CHECK-STATUS.

      CHECK-BALANCE.
          DISPLAY 'Checking balance...'.
          CALL 'CALCULATOR' USING WS-TOTAL.

      CHECK-STATUS.
          DISPLAY 'Checking status...'.

      UPDATE-DATABASE.
          DISPLAY 'Updating database...'.
          CALL 'DBUTIL' USING 'UPDATE' 'CUSTOMER'.
          PERFORM AUDIT-LOG.

      AUDIT-LOG.
          DISPLAY 'Writing audit log...'.
          CALL 'LOGGER' USING 'AUDIT' 'Customer updated'.

      GENERATE-REPORT.
          DISPLAY 'Generating report...'.
          PERFORM FORMAT-OUTPUT.
          CALL 'PRINTER' USING 'PRINT' 'REPORT'.

      FORMAT-OUTPUT.
          DISPLAY 'Formatting output...'.
          CALL WS-SUBPROG-NAME USING WS-TOTAL.
"""

    # Save to file
    demo_file = Path("demo_customer_processor.cbl")
    demo_file.write_text(program)
    return demo_file


def main():
    print("=" * 80)
    print("Call Graph Analysis Demo")
    print("=" * 80)
    print()

    # Create sample program
    demo_file = create_sample_program()
    print(f"Created sample program: {demo_file}")
    print()

    # Step 1: Extract call sites
    print("=" * 80)
    print("Step 1: Extracting Call Sites")
    print("=" * 80)
    print()

    parser = COBOLParser()
    extractor = CallExtractor(parser)

    call_sites = extractor.extract_from_file(str(demo_file))

    print(f"Found {len(call_sites)} call sites:\n")
    for i, call_site in enumerate(call_sites, 1):
        print(f"{i:2d}. {call_site}")

    print()

    # Show statistics
    stats = extractor.get_statistics(call_sites)
    print("Statistics:")
    for key, value in stats.items():
        print(f"  {key}: {value}")
    print()

    # Step 2: Build call graph
    print("=" * 80)
    print("Step 2: Building Call Graph")
    print("=" * 80)
    print()

    builder = CallGraphBuilder()
    graph = builder.build(call_sites)

    print(f"Built call graph: {graph}")
    print()

    print(f"Nodes ({len(graph.nodes)}):")
    for node_id, node in graph.nodes.items():
        print(f"  • {node.name:20s} {node.metrics}")

    print()
    print(f"Entry points: {len(graph.entry_points)}")
    for ep in graph.entry_points:
        node = graph.get_node(ep)
        if node:
            print(f"  • {node.name}")

    if graph.orphaned_nodes:
        print()
        print(f"Orphaned nodes (dead code): {len(graph.orphaned_nodes)}")
        for orphan_id in graph.orphaned_nodes:
            node = graph.get_node(orphan_id)
            if node:
                print(f"  • {node.name}")

    print()

    # Step 3: Impact analysis
    print("=" * 80)
    print("Step 3: Impact Analysis")
    print("=" * 80)
    print()

    analyzer = CallGraphAnalyzer(graph)

    # Analyze impact of changing a critical procedure
    test_nodes = [
        "CUSTOMER-PROCESSOR.VALIDATE-CUSTOMER",
        "CUSTOMER-PROCESSOR.CHECK-BALANCE",
    ]

    for target_node in test_nodes:
        if target_node in graph.nodes:
            print(f"Impact of changing '{target_node}':")
            print("-" * 60)

            analysis = analyzer.analyze_impact(target_node)

            print(f"  Target: {analysis.target_name}")
            print(f"  Risk Level: {analysis.risk_level}")
            print(f"  Risk Score: {analysis.risk_score:.1f}/100")
            print(f"  Total Impact: {analysis.total_impact} nodes affected")
            print()

            if analysis.directly_affected:
                print(f"  Direct callers ({len(analysis.directly_affected)}):")
                for caller in analysis.directly_affected:
                    node = graph.get_node(caller)
                    if node:
                        print(f"    • {node.name}")
                print()

            if analysis.transitively_affected:
                print(f"  Transitive callers ({len(analysis.transitively_affected)}):")
                for caller in analysis.transitively_affected[:5]:
                    node = graph.get_node(caller)
                    if node:
                        print(f"    • {node.name}")
                if len(analysis.transitively_affected) > 5:
                    print(f"    ... and {len(analysis.transitively_affected) - 5} more")
                print()

            if analysis.warnings:
                print("  Warnings:")
                for warning in analysis.warnings:
                    print(f"    {warning}")
                print()

            if analysis.recommendations:
                print("  Recommendations:")
                for rec in analysis.recommendations:
                    print(f"    {rec}")
                print()

            print()

    # Step 4: Find hot spots
    print("=" * 80)
    print("Step 4: Hot Spot Analysis")
    print("=" * 80)
    print()

    hot_spots = analyzer.find_hot_spots(top_n=5)
    print("Most called nodes (hot spots):")
    for node in hot_spots:
        if node.metrics.fan_in > 0:
            print(f"  • {node.name:20s} (called {node.metrics.fan_in} times)")
    print()

    # Step 5: Dead code detection
    dead_code = analyzer.find_dead_code()
    if dead_code:
        print("Dead code detected:")
        for node_id in dead_code:
            node = graph.get_node(node_id)
            if node:
                print(f"  • {node.name}")
        print()

    # Step 6: Circular dependencies
    cycles = analyzer.find_circular_dependencies()
    if cycles:
        print(f"Circular dependencies found: {len(cycles)}")
        for i, cycle in enumerate(cycles, 1):
            print(f"  Cycle {i}: {' → '.join(cycle)}")
        print()
    else:
        print("No circular dependencies detected ✓")
        print()

    # Step 7: Visualization
    print("=" * 80)
    print("Step 5: Visualization")
    print("=" * 80)
    print()

    visualizer = CallGraphVisualizer(graph)

    # Generate summary
    print(visualizer.generate_summary())

    # Generate ASCII tree
    print("=" * 80)
    print("Call Graph Tree (ASCII)")
    print("=" * 80)
    print()
    print(visualizer.to_ascii(max_depth=3))

    # Export to DOT format
    dot_file = Path("demo_callgraph.dot")
    dot_content = visualizer.to_dot(show_metrics=True, show_line_numbers=True)
    dot_file.write_text(dot_content)
    print(f"Exported GraphViz DOT format to: {dot_file}")
    print("  (Use 'dot -Tpng demo_callgraph.dot -o callgraph.png' to generate image)")
    print()

    # Export to JSON
    import json
    json_file = Path("demo_callgraph.json")
    json_content = visualizer.to_json()
    json_file.write_text(json.dumps(json_content, indent=2))
    print(f"Exported JSON format to: {json_file}")
    print()

    print("=" * 80)
    print("Demo Complete!")
    print("=" * 80)
    print()

    print("Summary:")
    print(f"  • Extracted {len(call_sites)} call sites")
    print(f"  • Built graph with {len(graph.nodes)} nodes and {len(graph.edges)} edges")
    print(f"  • Identified {len(graph.entry_points)} entry points")
    print(f"  • Found {len(dead_code)} dead code nodes")
    print(f"  • Max call depth: {graph.max_depth}")
    print()

    # Cleanup
    demo_file.unlink()


if __name__ == '__main__':
    main()
