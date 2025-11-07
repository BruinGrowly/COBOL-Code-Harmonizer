#!/usr/bin/env python3
"""
Integrated Demo: Copybook Resolution + Call Graph Analysis

Shows the complete Phase 4A Enterprise Features working together:
1. Copybook resolution (with nested copybooks)
2. Call graph extraction from resolved source
3. Impact analysis on enterprise COBOL programs
"""

from pathlib import Path
import json

from cobol_harmonizer.copybook import (
    CopybookConfig,
    CopybookResolver,
)
from cobol_harmonizer.callgraph import (
    CallExtractor,
    CallGraphBuilder,
    CallGraphAnalyzer,
)
from cobol_harmonizer.callgraph.visualizer import CallGraphVisualizer
from cobol_harmonizer.parser import COBOLParser


def create_enterprise_program():
    """Create a realistic enterprise COBOL program with copybooks"""
    program = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. ACCT-PROCESSOR.

      DATA DIVISION.
      WORKING-STORAGE SECTION.
      COPY CUSTOMER-RECORD.
      COPY ACCOUNT-RECORD.

      01  WS-STATUS               PIC X.
          88  WS-SUCCESS          VALUE 'Y'.
          88  WS-FAILURE          VALUE 'N'.

      PROCEDURE DIVISION.
      MAIN-PROCESSING.
          PERFORM INITIALIZE-SYSTEM.
          PERFORM PROCESS-ACCOUNTS UNTIL WS-STATUS = 'X'.
          PERFORM CLEANUP-SYSTEM.
          STOP RUN.

      INITIALIZE-SYSTEM.
          DISPLAY 'Initializing account processing system...'.
          CALL 'LOGGER' USING 'INFO' 'System started'.
          PERFORM LOAD-CONFIG.
          PERFORM OPEN-FILES.

      LOAD-CONFIG.
          DISPLAY 'Loading configuration...'.
          CALL 'CONFIG-UTIL' USING 'LOAD' WS-STATUS.

      OPEN-FILES.
          DISPLAY 'Opening files...'.
          CALL 'FILE-MANAGER' USING 'OPEN' 'ACCOUNTS'.

      PROCESS-ACCOUNTS.
          DISPLAY 'Processing accounts...'.
          PERFORM READ-ACCOUNT.
          PERFORM VALIDATE-ACCOUNT.
          PERFORM UPDATE-ACCOUNT.
          PERFORM WRITE-AUDIT.

      READ-ACCOUNT.
          DISPLAY 'Reading account...'.
          CALL 'FILE-MANAGER' USING 'READ' 'ACCOUNTS'.
          MOVE ACCT-NUMBER TO WS-STATUS.

      VALIDATE-ACCOUNT.
          DISPLAY 'Validating account...'.
          PERFORM CHECK-ACCOUNT-TYPE.
          PERFORM CHECK-BALANCE.
          PERFORM VERIFY-CUSTOMER.

      CHECK-ACCOUNT-TYPE.
          DISPLAY 'Checking account type...'.
          IF ACCT-TYPE = 'CHECKING' OR 'SAVINGS'
              SET WS-SUCCESS TO TRUE
          ELSE
              SET WS-FAILURE TO TRUE.

      CHECK-BALANCE.
          DISPLAY 'Checking balance...'.
          CALL 'CALC-ENGINE' USING ACCT-BALANCE.
          IF ACCT-BALANCE < 0
              PERFORM HANDLE-OVERDRAFT.

      HANDLE-OVERDRAFT.
          DISPLAY 'Handling overdraft...'.
          CALL 'OVERDRAFT-PROCESSOR' USING ACCT-NUMBER.

      VERIFY-CUSTOMER.
          DISPLAY 'Verifying customer...'.
          CALL 'CUST-VERIFY' USING CUST-ID CUST-NAME.

      UPDATE-ACCOUNT.
          DISPLAY 'Updating account...'.
          CALL 'DB-MANAGER' USING 'UPDATE' ACCOUNT-RECORD.

      WRITE-AUDIT.
          DISPLAY 'Writing audit log...'.
          CALL 'LOGGER' USING 'AUDIT' ACCT-NUMBER.

      CLEANUP-SYSTEM.
          DISPLAY 'Cleaning up...'.
          PERFORM CLOSE-FILES.
          CALL 'LOGGER' USING 'INFO' 'System shutdown'.

      CLOSE-FILES.
          DISPLAY 'Closing files...'.
          CALL 'FILE-MANAGER' USING 'CLOSE' 'ACCOUNTS'.
"""

    demo_file = Path("demo_acct_processor.cbl")
    demo_file.write_text(program)
    return demo_file


def main():
    print("=" * 80)
    print("PHASE 4A INTEGRATED DEMO")
    print("Copybook Resolution + Call Graph Analysis")
    print("=" * 80)
    print()

    # Create sample program
    demo_file = create_enterprise_program()
    print(f"Created enterprise program: {demo_file}")
    print()

    # ========================================================================
    # PART 1: Copybook Resolution
    # ========================================================================

    print("=" * 80)
    print("PART 1: Copybook Resolution")
    print("=" * 80)
    print()

    # Configure resolver
    config = CopybookConfig(
        search_paths=['./test_copybooks'],
        enable_cache=True,
    )

    resolver = CopybookResolver(config)

    print("Resolving copybooks...")
    resolved = resolver.resolve_file(str(demo_file))

    print(f"✓ Resolved {len(resolved.copybooks_used)} copybooks in {resolved.resolution_time_ms:.1f}ms")
    print()

    print("Copybooks used:")
    for copybook in resolved.copybooks_used:
        lines = copybook.content.count('\n') + 1
        print(f"  • {copybook.name:20s} ({lines} lines) - {copybook.path}")
    print()

    print("Source statistics:")
    print(f"  Original lines: {resolved.total_lines}")
    print(f"  Lines from copybooks: {resolved.total_lines_from_copybooks}")
    print(f"  Percentage from copybooks: {(resolved.total_lines_from_copybooks / resolved.total_lines * 100):.1f}%")
    print()

    # ========================================================================
    # PART 2: Call Graph Analysis on Resolved Source
    # ========================================================================

    print("=" * 80)
    print("PART 2: Call Graph Analysis")
    print("=" * 80)
    print()

    # Extract call sites from RESOLVED source
    parser = COBOLParser()
    extractor = CallExtractor(parser)

    # Parse the resolved source
    program = parser.parse_source(resolved.resolved_content)
    call_sites = extractor.extract_from_source(
        resolved.resolved_content,
        program.program_id,
        str(demo_file)
    )

    stats = extractor.get_statistics(call_sites)

    print(f"✓ Extracted {len(call_sites)} call sites")
    print()

    print("Call site breakdown:")
    print(f"  Inter-program calls (CALL): {stats['program_calls']}")
    print(f"  Dynamic calls: {stats['dynamic_calls']}")
    print(f"  Intra-program calls (PERFORM): {stats['perform_statements']}")
    print()

    # Build call graph
    builder = CallGraphBuilder()
    graph = builder.build(call_sites)

    print(f"✓ Built call graph: {len(graph.nodes)} nodes, {len(graph.edges)} edges")
    print()

    # ========================================================================
    # PART 3: Impact Analysis
    # ========================================================================

    print("=" * 80)
    print("PART 3: Impact Analysis")
    print("=" * 80)
    print()

    analyzer = CallGraphAnalyzer(graph)

    # Analyze a critical procedure
    critical_procedures = [
        "ACCT-PROCESSOR.VALIDATE-ACCOUNT",
        "ACCT-PROCESSOR.CHECK-BALANCE",
        "ACCT-PROCESSOR.UPDATE-ACCOUNT",
    ]

    print("Analyzing impact of changing critical procedures:")
    print()

    for proc_name in critical_procedures:
        if proc_name in graph.nodes:
            analysis = analyzer.analyze_impact(proc_name)

            print(f"📊 {analysis.target_name}")
            print(f"   Risk: {analysis.risk_level} ({analysis.risk_score:.1f}/100)")
            print(f"   Impact: {analysis.total_impact} nodes affected")

            if analysis.directly_affected:
                print(f"   Direct callers: {len(analysis.directly_affected)}")

            if analysis.warnings:
                print(f"   ⚠️  {len(analysis.warnings)} warning(s)")

            print()

    # ========================================================================
    # PART 4: Code Quality Analysis
    # ========================================================================

    print("=" * 80)
    print("PART 4: Code Quality Analysis")
    print("=" * 80)
    print()

    # Find hot spots
    hot_spots = analyzer.find_hot_spots(top_n=5)
    print("Hot spots (most frequently called):")
    for node in hot_spots:
        if node.metrics.fan_in > 0:
            print(f"  • {node.name:25s} (called {node.metrics.fan_in}x)")
    print()

    # Check for circular dependencies
    cycles = analyzer.find_circular_dependencies()
    if cycles:
        print(f"⚠️  Circular dependencies found: {len(cycles)}")
        for cycle in cycles:
            print(f"   {' → '.join(cycle)}")
    else:
        print("✓ No circular dependencies detected")
    print()

    # Check for dead code
    dead_code = analyzer.find_dead_code()
    if dead_code:
        print(f"⚠️  Dead code detected: {len(dead_code)} unreachable procedures")
        for node_id in dead_code[:5]:
            node = graph.get_node(node_id)
            if node:
                print(f"   • {node.name}")
    else:
        print("✓ No dead code detected")
    print()

    # ========================================================================
    # PART 5: Visualization Export
    # ========================================================================

    print("=" * 80)
    print("PART 5: Visualization Export")
    print("=" * 80)
    print()

    visualizer = CallGraphVisualizer(graph)

    # Export to various formats
    dot_file = Path("integrated_callgraph.dot")
    dot_content = visualizer.to_dot(show_metrics=True)
    dot_file.write_text(dot_content)
    print(f"✓ Exported GraphViz DOT: {dot_file}")

    json_file = Path("integrated_callgraph.json")
    json_content = visualizer.to_json()
    json_file.write_text(json.dumps(json_content, indent=2))
    print(f"✓ Exported JSON: {json_file}")

    ascii_file = Path("integrated_callgraph.txt")
    ascii_content = visualizer.to_ascii(max_depth=4)
    ascii_file.write_text(ascii_content)
    print(f"✓ Exported ASCII tree: {ascii_file}")
    print()

    # ========================================================================
    # SUMMARY
    # ========================================================================

    print("=" * 80)
    print("DEMO COMPLETE - Phase 4A Features Verified!")
    print("=" * 80)
    print()

    print("✅ COPYBOOK RESOLUTION:")
    print(f"   • Resolved {len(resolved.copybooks_used)} copybooks")
    print(f"   • Expanded {resolved.total_lines_from_copybooks} lines")
    print(f"   • Resolution time: {resolved.resolution_time_ms:.1f}ms")
    print()

    print("✅ CALL GRAPH ANALYSIS:")
    print(f"   • Extracted {len(call_sites)} call sites")
    print(f"   • Built graph with {len(graph.nodes)} nodes, {len(graph.edges)} edges")
    print(f"   • Max call depth: {graph.max_depth}")
    print()

    print("✅ IMPACT ANALYSIS:")
    print(f"   • Analyzed {len(critical_procedures)} critical procedures")
    print(f"   • Identified entry points: {len(graph.entry_points)}")
    print(f"   • Detected dead code: {len(dead_code)} nodes")
    print()

    print("✅ VISUALIZATION:")
    print("   • GraphViz DOT format")
    print("   • JSON for web visualization")
    print("   • ASCII tree for terminal")
    print()

    print("=" * 80)
    print()

    print("Next steps:")
    print("  1. Generate call graph image:")
    print("     dot -Tpng integrated_callgraph.dot -o callgraph.png")
    print()
    print("  2. View ASCII tree:")
    print("     cat integrated_callgraph.txt")
    print()
    print("  3. Analyze JSON data:")
    print("     cat integrated_callgraph.json | jq .")
    print()

    # Cleanup
    demo_file.unlink()


if __name__ == '__main__':
    main()
