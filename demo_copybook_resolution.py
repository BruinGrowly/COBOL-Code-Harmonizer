#!/usr/bin/env python3
"""
Demo: Copybook Resolution System

Shows how the copybook resolver works
"""

from cobol_harmonizer.copybook import (
    CopybookConfig,
    CopybookResolver,
)
from pathlib import Path


def main():
    print("=" * 80)
    print("Copybook Resolution Demo")
    print("=" * 80)
    print()

    # Create a simple COBOL program with COPY statements
    demo_program = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. CUSTOMER-UPDATE.

      DATA DIVISION.
      WORKING-STORAGE SECTION.
      COPY CUSTOMER-RECORD.
      COPY ACCOUNT-RECORD.

      01  WS-COUNTER              PIC 9(6) VALUE ZERO.

      PROCEDURE DIVISION.
      MAIN-PROCEDURE.
          DISPLAY 'Processing customer update...'.
          MOVE 123456 TO CUST-ID.
          MOVE 'John Smith' TO CUST-NAME.
          MOVE 5000.00 TO ACCT-BALANCE.
          STOP RUN.
"""

    # Save demo program
    demo_file = Path("demo_program.cbl")
    demo_file.write_text(demo_program)

    # Configure resolver
    config = CopybookConfig(
        search_paths=['./test_copybooks'],
        enable_cache=True,
    )

    print("Configuration:")
    print(f"  Search paths: {config.search_paths}")
    print(f"  Cache enabled: {config.enable_cache}")
    print()

    # Create resolver
    resolver = CopybookResolver(config)

    print("Resolving copybooks in demo_program.cbl...")
    print()

    # Resolve copybooks
    resolved = resolver.resolve_file(str(demo_file))

    # Display results
    print("=" * 80)
    print("Resolution Results")
    print("=" * 80)
    print()

    print(f"Original file: {resolved.original_path}")
    print(f"Total lines: {resolved.total_lines}")
    print(f"Lines from copybooks: {resolved.total_lines_from_copybooks}")
    print(f"Copybooks used: {len(resolved.copybooks_used)}")
    print(f"Resolution time: {resolved.resolution_time_ms:.1f}ms")
    print()

    print("Copybooks resolved:")
    for copybook in resolved.copybooks_used:
        print(f"  • {copybook.name}")
        print(f"    Path: {copybook.path}")
        print(f"    Lines: {copybook.content.count(chr(10)) + 1}")
    print()

    print("=" * 80)
    print("Resolved Source (first 50 lines)")
    print("=" * 80)
    lines = resolved.resolved_content.split('\n')
    for i, line in enumerate(lines[:50], 1):
        # Show source location
        location = resolved.source_map.get_location(i)
        marker = "📋" if (location and location.is_copybook) else "  "
        print(f"{marker} {i:3d}: {line}")

    if len(lines) > 50:
        print(f"\n... ({len(lines) - 50} more lines) ...")

    print()
    print("=" * 80)
    print("Cache Statistics")
    print("=" * 80)
    stats = resolver.cache.get_stats()
    for key, value in stats.items():
        print(f"  {key}: {value}")

    print()
    print("✅ Copybook resolution demo complete!")
    print()

    # Cleanup
    demo_file.unlink()


if __name__ == '__main__':
    main()
