#!/usr/bin/env python3
"""
Test COBOL Code Harmonizer on all 30 variant samples
Validates variant detection accuracy and collects results
"""

import os
import sys
import json
from pathlib import Path
from cobol_harmonizer.parser.cobol_parser import COBOLParser, COBOLFormat, COBOLStandard, COBOLDialect

# Sample test matrix
SAMPLES = [
    # COBOL-74 (3 samples)
    ('variant_test_samples/cobol74/payroll_legacy.cbl', COBOLStandard.COBOL_74, COBOLFormat.FIXED, 'COBOL-74'),
    ('variant_test_samples/cobol74/bank_transaction.cbl', COBOLStandard.COBOL_74, COBOLFormat.FIXED, 'COBOL-74'),
    ('variant_test_samples/cobol74/inventory_control.cbl', COBOLStandard.COBOL_74, COBOLFormat.FIXED, 'COBOL-74'),

    # COBOL-85 (4 samples - from external_samples)
    ('external_samples/banking_system.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'COBOL-85'),
    ('external_samples/data_validation.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'COBOL-85'),
    ('external_samples/db2_client.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'COBOL-85'),
    ('external_samples/json_parse.cbl', COBOLStandard.COBOL_2014, COBOLFormat.FIXED, 'COBOL-2014'),

    # COBOL-2002 OOP (3 samples)
    ('variant_test_samples/cobol2002_oop/customer_class.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FIXED, 'COBOL-2002'),
    ('variant_test_samples/cobol2002_oop/account_manager.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FIXED, 'COBOL-2002'),
    ('variant_test_samples/cobol2002_oop/order_processor.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FIXED, 'COBOL-2002'),

    # COBOL-2014 (2 samples)
    ('variant_test_samples/cobol2014/api_gateway.cbl', COBOLStandard.COBOL_2014, COBOLFormat.FIXED, 'COBOL-2014'),
    ('variant_test_samples/cobol2014/financial_calculator.cbl', COBOLStandard.COBOL_2014, COBOLFormat.FIXED, 'COBOL-2014'),

    # IBM Mainframe (3 samples)
    ('variant_test_samples/ibm_mainframe/db2_customer_query.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'IBM'),
    ('variant_test_samples/ibm_mainframe/cics_transaction.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'IBM'),
    ('variant_test_samples/ibm_mainframe/mixed_sql_cics.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'IBM'),

    # GnuCOBOL (3 samples)
    ('variant_test_samples/gnucobol/modern_inventory.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FREE, 'GnuCOBOL'),
    ('variant_test_samples/gnucobol/web_service_handler.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FREE, 'GnuCOBOL'),
    ('variant_test_samples/gnucobol/string_processor.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FREE, 'GnuCOBOL'),

    # Micro Focus (3 samples)
    ('variant_test_samples/micro_focus/dotnet_integration.cbl', COBOLStandard.COBOL_2002, COBOLFormat.FREE, 'Micro Focus'),
    ('variant_test_samples/micro_focus/screen_handler.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Micro Focus'),
    ('variant_test_samples/micro_focus/mixed_format_demo.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Micro Focus'),

    # Financial Services (3 samples)
    ('variant_test_samples/financial_services/portfolio_manager.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Financial'),
    ('variant_test_samples/financial_services/credit_card_processor.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Financial'),
    ('variant_test_samples/financial_services/atm_controller.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Financial'),

    # Insurance (3 samples)
    ('variant_test_samples/insurance/claims_adjudication.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Insurance'),
    ('variant_test_samples/insurance/policy_administration.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Insurance'),
    ('variant_test_samples/insurance/premium_calculator.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Insurance'),

    # Government (3 samples)
    ('variant_test_samples/government/tax_calculation.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Government'),
    ('variant_test_samples/government/social_security.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Government'),
    ('variant_test_samples/government/healthcare_enrollment.cbl', COBOLStandard.COBOL_85, COBOLFormat.FIXED, 'Government'),
]

def test_variant_detection():
    """Test variant detection accuracy on all samples"""
    parser = COBOLParser()

    results = {
        'total_samples': len(SAMPLES),
        'successful_parses': 0,
        'failed_parses': 0,
        'format_detection_correct': 0,
        'standard_detection_correct': 0,
        'detailed_results': []
    }

    print("=" * 80)
    print("COBOL Code Harmonizer - Variant Detection Test")
    print("=" * 80)
    print(f"Testing {len(SAMPLES)} samples across all COBOL variants\n")

    for idx, (filepath, expected_standard, expected_format, category) in enumerate(SAMPLES, 1):
        print(f"[{idx:2d}/{len(SAMPLES)}] Testing: {os.path.basename(filepath)}")
        print(f"         Category: {category}")

        try:
            # Parse the file
            program = parser.parse_file(filepath)

            # Check detection accuracy
            format_correct = program.source_format == expected_format
            standard_correct = program.standard == expected_standard

            results['successful_parses'] += 1
            if format_correct:
                results['format_detection_correct'] += 1
            if standard_correct:
                results['standard_detection_correct'] += 1

            # Display results
            print(f"         Format: {program.source_format.value} "
                  f"({'✓' if format_correct else '✗ expected ' + expected_format.value})")
            print(f"         Standard: {program.standard.value} "
                  f"({'✓' if standard_correct else '✗ expected ' + expected_standard.value})")
            print(f"         Dialect: {program.dialect.value}")
            print(f"         Procedures: {len(program.procedures)}")

            if program.has_oop_features:
                print(f"         Features: OOP")
            if program.has_exec_sql:
                print(f"         Features: EXEC SQL")
            if program.has_exec_cics:
                print(f"         Features: EXEC CICS")
            if program.has_xml_json:
                print(f"         Features: XML/JSON")

            results['detailed_results'].append({
                'file': filepath,
                'category': category,
                'success': True,
                'detected_format': program.source_format.value,
                'expected_format': expected_format.value,
                'format_correct': format_correct,
                'detected_standard': program.standard.value,
                'expected_standard': expected_standard.value,
                'standard_correct': standard_correct,
                'dialect': program.dialect.value,
                'procedures': len(program.procedures),
                'features': {
                    'oop': program.has_oop_features,
                    'exec_sql': program.has_exec_sql,
                    'exec_cics': program.has_exec_cics,
                    'xml_json': program.has_xml_json
                }
            })

            print(f"         Status: ✓ SUCCESS\n")

        except Exception as e:
            results['failed_parses'] += 1
            print(f"         Status: ✗ FAILED - {str(e)}\n")

            results['detailed_results'].append({
                'file': filepath,
                'category': category,
                'success': False,
                'error': str(e)
            })

    return results

def display_summary(results):
    """Display summary of test results"""
    print("\n" + "=" * 80)
    print("TEST SUMMARY")
    print("=" * 80)

    print(f"\nParsing Results:")
    print(f"  Total Samples:      {results['total_samples']}")
    print(f"  Successful Parses:  {results['successful_parses']} "
          f"({results['successful_parses']/results['total_samples']*100:.1f}%)")
    print(f"  Failed Parses:      {results['failed_parses']}")

    print(f"\nDetection Accuracy:")
    successful = results['successful_parses']
    if successful > 0:
        format_pct = results['format_detection_correct'] / successful * 100
        standard_pct = results['standard_detection_correct'] / successful * 100

        print(f"  Format Detection:   {results['format_detection_correct']}/{successful} "
              f"({format_pct:.1f}%)")
        print(f"  Standard Detection: {results['standard_detection_correct']}/{successful} "
              f"({standard_pct:.1f}%)")

    # Summary by category
    print(f"\nResults by Category:")
    categories = {}
    for result in results['detailed_results']:
        cat = result['category']
        if cat not in categories:
            categories[cat] = {'total': 0, 'success': 0}
        categories[cat]['total'] += 1
        if result['success']:
            categories[cat]['success'] += 1

    for cat in sorted(categories.keys()):
        stats = categories[cat]
        print(f"  {cat:15s}: {stats['success']}/{stats['total']} samples")

    print()

def save_results(results):
    """Save results to JSON file"""
    output_file = 'variant_detection_results.json'
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"Detailed results saved to: {output_file}")

if __name__ == '__main__':
    results = test_variant_detection()
    display_summary(results)
    save_results(results)

    # Exit with error code if any tests failed
    sys.exit(0 if results['failed_parses'] == 0 else 1)
