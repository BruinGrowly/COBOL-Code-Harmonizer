"""
Tests for Config
"""

import pytest
import json
import tempfile
from pathlib import Path
from cobol_harmonizer.config import (
    Config,
    create_default_config_file,
    get_config_with_overrides,
)


class TestConfig:
    """Test suite for Config"""

    def test_default_config(self):
        """Test default configuration"""
        config = Config()

        # Check default values
        assert config.get("analysis.threshold") == 0.5
        assert config.get("batch.max_workers") == 4
        assert config.get("reporting.default_format") == "text"

    def test_config_with_dict(self):
        """Test config initialization with dictionary"""
        custom_config = {"analysis": {"threshold": 0.8}}

        config = Config(custom_config)

        assert config.get("analysis.threshold") == 0.8
        # Default values should still be present
        assert config.get("batch.max_workers") == 4

    def test_deep_merge(self):
        """Test deep merging of configurations"""
        base = {"a": {"b": 1, "c": 2}, "d": 3}

        override = {"a": {"c": 99}, "e": 4}

        merged = Config._deep_merge(base, override)

        assert merged["a"]["b"] == 1  # Preserved from base
        assert merged["a"]["c"] == 99  # Overridden
        assert merged["d"] == 3  # Preserved from base
        assert merged["e"] == 4  # Added from override

    def test_get_nested_value(self):
        """Test getting nested values with dot notation"""
        config = Config()

        # Get existing value
        threshold = config.get("analysis.threshold")
        assert threshold == 0.5

        # Get nested value
        max_workers = config.get("batch.max_workers")
        assert max_workers == 4

        # Get non-existent value with default
        value = config.get("nonexistent.key", default=42)
        assert value == 42

    def test_set_nested_value(self):
        """Test setting nested values with dot notation"""
        config = Config()

        # Set existing value
        config.set("analysis.threshold", 0.9)
        assert config.get("analysis.threshold") == 0.9

        # Set new value
        config.set("custom.new.value", 123)
        assert config.get("custom.new.value") == 123

    def test_update(self):
        """Test updating configuration"""
        config = Config()

        updates = {"analysis": {"threshold": 0.7}, "custom": {"setting": "value"}}

        config.update(updates)

        assert config.get("analysis.threshold") == 0.7
        assert config.get("custom.setting") == "value"

    def test_to_dict(self):
        """Test converting config to dictionary"""
        config = Config()
        config_dict = config.to_dict()

        assert isinstance(config_dict, dict)
        assert "analysis" in config_dict
        assert "batch" in config_dict

    def test_to_json(self):
        """Test converting config to JSON"""
        config = Config()
        json_str = config.to_json()

        # Should be valid JSON
        parsed = json.loads(json_str)
        assert isinstance(parsed, dict)

    def test_save_and_load_json(self):
        """Test saving and loading JSON config"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_path = Path(tmpdir) / "config.json"

            # Create and save config
            config = Config()
            config.set("analysis.threshold", 0.75)
            config.save(str(config_path), format="json")

            # Load config
            loaded_config = Config.from_file(str(config_path))

            assert loaded_config.get("analysis.threshold") == 0.75

    def test_validate_valid_config(self):
        """Test validation with valid config"""
        config = Config()
        errors = config.validate()

        assert len(errors) == 0

    def test_validate_invalid_threshold(self):
        """Test validation with invalid threshold"""
        config = Config()
        config.set("analysis.threshold", -1)

        errors = config.validate()

        assert len(errors) > 0
        assert any("threshold" in err.lower() for err in errors)

    def test_validate_invalid_max_workers(self):
        """Test validation with invalid max_workers"""
        config = Config()
        config.set("batch.max_workers", 0)

        errors = config.validate()

        assert len(errors) > 0
        assert any("max_workers" in err.lower() for err in errors)

    def test_get_severity_threshold(self):
        """Test getting severity thresholds"""
        config = Config()

        harmonious_max = config.get_severity_threshold("harmonious")
        assert harmonious_max == 0.3

        critical_max = config.get_severity_threshold("critical")
        assert critical_max == 999.0

    def test_should_take_action(self):
        """Test action required check"""
        config = Config()

        # Harmonious shouldn't require action
        assert not config.should_take_action("harmonious")

        # Critical should require action
        assert config.should_take_action("critical")

    def test_discover_config(self):
        """Test config discovery"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create config file
            config_file = tmppath / ".harmonizerrc.json"
            test_config = {"analysis": {"threshold": 0.88}}

            with open(config_file, "w") as f:
                json.dump(test_config, f)

            # Discover from that directory
            discovered = Config.discover(str(tmppath))

            assert discovered.get("analysis.threshold") == 0.88

    def test_discover_no_config(self):
        """Test discovery when no config file exists"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Should return default config
            config = Config.discover(str(tmpdir))

            assert config.get("analysis.threshold") == 0.5

    def test_create_default_config_file(self):
        """Test creating default config file"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_path = Path(tmpdir) / ".harmonizerrc.json"

            created_path = create_default_config_file(str(config_path))

            assert Path(created_path).exists()

            # Load and verify
            config = Config.from_file(created_path)
            assert config.get("analysis.threshold") == 0.5

    def test_get_config_with_overrides(self):
        """Test getting config with overrides"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / "config.json"

            # Create config file
            base_config = {"analysis": {"threshold": 0.6}}
            with open(config_file, "w") as f:
                json.dump(base_config, f)

            # Get with overrides
            overrides = {"analysis": {"threshold": 0.9}}
            config = get_config_with_overrides(str(config_file), overrides)

            assert config.get("analysis.threshold") == 0.9

    def test_config_file_priority(self):
        """Test config file search priority"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            # Create multiple config files
            (tmppath / ".harmonizerrc").write_text(
                json.dumps({"analysis": {"threshold": 0.1}})
            )
            (tmppath / "harmonizer.json").write_text(
                json.dumps({"analysis": {"threshold": 0.2}})
            )

            # Should prefer .harmonizerrc (higher priority)
            config = Config.discover(str(tmppath))

            # .harmonizerrc has highest priority
            assert config.get("analysis.threshold") == 0.1

    def test_exclude_patterns(self):
        """Test exclude patterns configuration"""
        config = Config()

        exclude_patterns = config.get("batch.exclude_patterns")

        assert isinstance(exclude_patterns, list)
        assert "**/node_modules/**" in exclude_patterns
        assert "**/.git/**" in exclude_patterns

    def test_file_patterns(self):
        """Test file patterns configuration"""
        config = Config()

        file_patterns = config.get("batch.file_patterns")

        assert isinstance(file_patterns, list)
        assert "*.cbl" in file_patterns
        assert "*.CBL" in file_patterns

    def test_ljpw_config(self):
        """Test LJPW configuration"""
        config = Config()

        anchor = config.get("ljpw.anchor_point")
        equilibrium = config.get("ljpw.natural_equilibrium")

        assert anchor == [1.0, 1.0, 1.0, 1.0]
        assert len(equilibrium) == 4
        assert equilibrium[0] == 0.618  # Golden ratio

    def test_reporting_config(self):
        """Test reporting configuration"""
        config = Config()

        assert config.get("reporting.default_format") == "text"
        assert config.get("reporting.include_suggestions") is True
        assert config.get("reporting.max_suggestions") == 3
