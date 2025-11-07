"""
Setup configuration for COBOL Code Harmonizer
"""

from setuptools import setup, find_packages
import os

# Read README for long description
def read_readme():
    readme_path = os.path.join(os.path.dirname(__file__), 'README.md')
    if os.path.exists(readme_path):
        with open(readme_path, 'r', encoding='utf-8') as f:
            return f.read()
    return ''

setup(
    name='cobol-harmonizer',
    version='0.1.0',
    description='Semantic analysis tool for COBOL using the LJPW framework',
    long_description=read_readme(),
    long_description_content_type='text/markdown',
    author='COBOL Code Harmonizer Contributors',
    author_email='',
    url='https://github.com/BruinGrowly/COBOL-Code-Harmonizer',
    license='MIT',
    packages=find_packages(exclude=['tests', 'tests.*', 'examples']),
    python_requires='>=3.8',
    install_requires=[
        'pyparsing>=3.0.0',
        'numpy>=1.21.0',
        'click>=8.0.0',
        'rich>=10.0.0',
    ],
    extras_require={
        'dev': [
            'pytest>=7.0.0',
            'pytest-cov>=3.0.0',
            'black>=22.0.0',
            'flake8>=4.0.0',
            'mypy>=0.950',
        ],
    },
    entry_points={
        'console_scripts': [
            'cobol-harmonizer=cobol_harmonizer.cli.commands:cli',
        ],
    },
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Topic :: Software Development :: Quality Assurance',
        'Topic :: Software Development :: Testing',
    ],
    keywords='cobol, static-analysis, semantic-analysis, code-quality, ljpw',
    project_urls={
        'Bug Reports': 'https://github.com/BruinGrowly/COBOL-Code-Harmonizer/issues',
        'Source': 'https://github.com/BruinGrowly/COBOL-Code-Harmonizer',
        'Documentation': 'https://github.com/BruinGrowly/COBOL-Code-Harmonizer/tree/main/docs',
    },
)
