"""
Configuration Management

Handles loading and validation of configuration from .harmonizerrc files.
Supports JSON and YAML formats.
"""

import json
from pathlib import Path
from typing import Dict, Optional, Any, List
import os


class Config:
    """Configuration management for COBOL Code Harmonizer"""

    # Default configuration
    DEFAULT_CONFIG = {
        "version": "0.1.0",
        "analysis": {
            "threshold": 0.5,
            "show_harmonious": False,
            "severity_levels": {
                "harmonious": {"max": 0.3, "action_required": False},
                "minor_drift": {"max": 0.5, "action_required": False},
                "concerning": {"max": 0.8, "action_required": True},
                "significant": {"max": 1.2, "action_required": True},
                "critical": {"max": 999.0, "action_required": True}
            }
        },
        "batch": {
            "max_workers": 4,
            "recursive": True,
            "file_patterns": ["*.cbl", "*.CBL", "*.cob", "*.COB", "*.cobol", "*.COBOL"],
            "exclude_patterns": [
                "**/node_modules/**",
                "**/vendor/**",
                "**/.git/**",
                "**/build/**",
                "**/dist/**"
            ]
        },
        "reporting": {
            "default_format": "text",
            "formats": ["text", "json", "sarif", "markdown"],
            "output_directory": "./harmonizer-reports",
            "include_suggestions": True,
            "show_trajectory": True,
            "max_suggestions": 3
        },
        "filters": {
            "min_severity": None,
            "max_procedures": None,
            "exclude_procedures": [],
            "include_only": []
        },
        "ljpw": {
            "anchor_point": [1.0, 1.0, 1.0, 1.0],
            "natural_equilibrium": [0.618, 0.414, 0.718, 0.693],
            "normalization": "unit_vector"
        }
    }

    # Config file names to search for (in order of precedence)
    CONFIG_FILENAMES = [
        ".harmonizerrc",
        ".harmonizerrc.json",
        ".harmonizer.json",
        "harmonizer.json",
        ".harmonizerrc.yml",
        ".harmonizerrc.yaml",
        "harmonizer.yml",
        "harmonizer.yaml"
    ]

    def __init__(self, config_dict: Optional[Dict] = None):
        """
        Initialize configuration.

        Args:
            config_dict: Optional configuration dictionary to use
        """
        self.config = self._deep_merge(
            self.DEFAULT_CONFIG.copy(),
            config_dict or {}
        )

    @classmethod
    def from_file(cls, config_path: str) -> 'Config':
        """
        Load configuration from file.

        Args:
            config_path: Path to configuration file

        Returns:
            Config instance
        """
        config_file = Path(config_path)

        if not config_file.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_path}")

        # Determine format from extension
        if config_file.suffix in ['.json']:
            config_dict = cls._load_json(config_file)
        elif config_file.suffix in ['.yml', '.yaml']:
            config_dict = cls._load_yaml(config_file)
        else:
            # Try JSON first, then YAML
            try:
                config_dict = cls._load_json(config_file)
            except json.JSONDecodeError:
                config_dict = cls._load_yaml(config_file)

        return cls(config_dict)

    @classmethod
    def discover(cls, start_path: Optional[str] = None) -> 'Config':
        """
        Discover and load configuration file.

        Searches for configuration files in the following order:
        1. Current directory
        2. Parent directories (up to home directory)
        3. Home directory

        Args:
            start_path: Directory to start searching from (default: current directory)

        Returns:
            Config instance (uses defaults if no config file found)
        """
        search_path = Path(start_path or os.getcwd()).resolve()
        home_path = Path.home()

        # Search from current directory up to home
        current = search_path
        while True:
            # Try each config filename
            for filename in cls.CONFIG_FILENAMES:
                config_file = current / filename
                if config_file.exists():
                    try:
                        return cls.from_file(str(config_file))
                    except Exception:
                        # If loading fails, continue searching
                        continue

            # Stop at home directory
            if current == home_path or current == current.parent:
                break

            current = current.parent

        # No config found, use defaults
        return cls()

    @staticmethod
    def _load_json(file_path: Path) -> Dict:
        """Load JSON configuration file"""
        with open(file_path, 'r', encoding='utf-8') as f:
            return json.load(f)

    @staticmethod
    def _load_yaml(file_path: Path) -> Dict:
        """Load YAML configuration file"""
        try:
            import yaml
            with open(file_path, 'r', encoding='utf-8') as f:
                return yaml.safe_load(f) or {}
        except ImportError:
            raise ImportError(
                "YAML support requires PyYAML. Install with: pip install pyyaml"
            )

    @staticmethod
    def _deep_merge(base: Dict, override: Dict) -> Dict:
        """Deep merge two dictionaries"""
        result = base.copy()

        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = Config._deep_merge(result[key], value)
            else:
                result[key] = value

        return result

    def get(self, key_path: str, default: Any = None) -> Any:
        """
        Get configuration value using dot notation.

        Args:
            key_path: Dot-separated path (e.g., 'analysis.threshold')
            default: Default value if key not found

        Returns:
            Configuration value
        """
        keys = key_path.split('.')
        value = self.config

        for key in keys:
            if isinstance(value, dict) and key in value:
                value = value[key]
            else:
                return default

        return value

    def set(self, key_path: str, value: Any):
        """
        Set configuration value using dot notation.

        Args:
            key_path: Dot-separated path (e.g., 'analysis.threshold')
            value: Value to set
        """
        keys = key_path.split('.')
        config = self.config

        for key in keys[:-1]:
            if key not in config:
                config[key] = {}
            config = config[key]

        config[keys[-1]] = value

    def update(self, updates: Dict):
        """
        Update configuration with dictionary.

        Args:
            updates: Dictionary of updates to apply
        """
        self.config = self._deep_merge(self.config, updates)

    def to_dict(self) -> Dict:
        """Get configuration as dictionary"""
        return self.config.copy()

    def to_json(self, indent: int = 2) -> str:
        """Get configuration as JSON string"""
        return json.dumps(self.config, indent=indent)

    def save(self, output_path: str, format: str = 'json'):
        """
        Save configuration to file.

        Args:
            output_path: Output file path
            format: Output format ('json' or 'yaml')
        """
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        if format == 'json':
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(self.config, f, indent=2)
        elif format == 'yaml':
            try:
                import yaml
                with open(output_file, 'w', encoding='utf-8') as f:
                    yaml.safe_dump(self.config, f, default_flow_style=False)
            except ImportError:
                raise ImportError(
                    "YAML support requires PyYAML. Install with: pip install pyyaml"
                )
        else:
            raise ValueError(f"Unsupported format: {format}")

    def validate(self) -> List[str]:
        """
        Validate configuration.

        Returns:
            List of validation errors (empty if valid)
        """
        errors = []

        # Validate threshold
        threshold = self.get('analysis.threshold')
        if threshold is not None:
            if not isinstance(threshold, (int, float)):
                errors.append("analysis.threshold must be a number")
            elif threshold < 0:
                errors.append("analysis.threshold must be non-negative")

        # Validate max_workers
        max_workers = self.get('batch.max_workers')
        if max_workers is not None:
            if not isinstance(max_workers, int):
                errors.append("batch.max_workers must be an integer")
            elif max_workers < 1:
                errors.append("batch.max_workers must be at least 1")

        # Validate file_patterns
        file_patterns = self.get('batch.file_patterns')
        if file_patterns is not None:
            if not isinstance(file_patterns, list):
                errors.append("batch.file_patterns must be a list")

        # Validate severity levels
        severity_levels = self.get('analysis.severity_levels')
        if severity_levels is not None:
            if not isinstance(severity_levels, dict):
                errors.append("analysis.severity_levels must be a dictionary")
            else:
                for level, settings in severity_levels.items():
                    if not isinstance(settings, dict):
                        errors.append(f"analysis.severity_levels.{level} must be a dictionary")
                    elif 'max' not in settings:
                        errors.append(f"analysis.severity_levels.{level} must have 'max' key")

        return errors

    def get_severity_threshold(self, severity: str) -> Optional[float]:
        """
        Get threshold for a specific severity level.

        Args:
            severity: Severity level name

        Returns:
            Threshold value or None
        """
        return self.get(f'analysis.severity_levels.{severity}.max')

    def should_take_action(self, severity: str) -> bool:
        """
        Check if action is required for a severity level.

        Args:
            severity: Severity level name

        Returns:
            True if action required
        """
        return self.get(f'analysis.severity_levels.{severity}.action_required', False)


def create_default_config_file(output_path: str = ".harmonizerrc.json"):
    """
    Create a default configuration file.

    Args:
        output_path: Output file path
    """
    config = Config()
    format_type = 'yaml' if output_path.endswith(('.yml', '.yaml')) else 'json'
    config.save(output_path, format=format_type)
    return output_path


def get_config_with_overrides(
    config_file: Optional[str] = None,
    overrides: Optional[Dict] = None
) -> Config:
    """
    Get configuration with optional file and overrides.

    Args:
        config_file: Optional path to config file
        overrides: Optional dictionary of overrides

    Returns:
        Config instance
    """
    # Load from file or discover
    if config_file:
        config = Config.from_file(config_file)
    else:
        config = Config.discover()

    # Apply overrides
    if overrides:
        config.update(overrides)

    # Validate
    errors = config.validate()
    if errors:
        raise ValueError(f"Invalid configuration: {', '.join(errors)}")

    return config
