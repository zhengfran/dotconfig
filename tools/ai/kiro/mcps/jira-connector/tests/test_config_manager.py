"""
Property-based tests for ConfigManager.

**Feature: dynamic-jira-config**

Uses Hypothesis for property-based testing with minimum 100 iterations per property.
"""
import os
import pytest
from hypothesis import given, settings, strategies as st

import sys
sys.path.insert(0, str(__file__).replace("tests/test_config_manager.py", "src"))

from jira_client import ConfigManager, JiraConfigProfile


# Strategy for valid config names (non-empty alphanumeric with underscores/hyphens)
config_name_strategy = st.text(
    alphabet=st.sampled_from("abcdefghijklmnopqrstuvwxyz0123456789_-"),
    min_size=1,
    max_size=20
).filter(lambda x: x.strip() != "")

# Strategy for valid URLs
url_strategy = st.text(
    alphabet=st.sampled_from("abcdefghijklmnopqrstuvwxyz0123456789.-:/"),
    min_size=10,
    max_size=50
).map(lambda x: f"https://{x}.example.com")

# Strategy for valid PATs (non-empty strings)
pat_strategy = st.text(min_size=10, max_size=50).filter(lambda x: x.strip() != "")


class TestConfigManagerProperties:
    """Property-based tests for ConfigManager."""

    @settings(max_examples=100)
    @given(name=config_name_strategy, url=url_strategy, pat=pat_strategy)
    def test_property_2_add_config_makes_it_retrievable(self, name: str, url: str, pat: str):
        """
        **Feature: dynamic-jira-config, Property 2: Add config makes it retrievable**
        
        For any valid configuration (name, URL, PAT), after calling add_config,
        the configuration should appear in list_configs with the correct name and URL.
        
        **Validates: Requirements 1.4, 2.1**
        """
        manager = ConfigManager(load_from_env=False)
        
        # Add the config
        result = manager.add_config(name, url, pat)
        assert result.success, f"add_config failed: {result.error}"
        
        # Verify it appears in list
        list_result = manager.list_configs()
        assert list_result.success
        
        profiles = list_result.data["profiles"]
        names = [p["name"] for p in profiles]
        assert name in names, f"Config '{name}' not found in list"
        
        # Verify URL matches
        profile = next(p for p in profiles if p["name"] == name)
        assert profile["base_url"] == url.rstrip("/")

    @settings(max_examples=100)
    @given(name=config_name_strategy, url1=url_strategy, pat1=pat_strategy, url2=url_strategy, pat2=pat_strategy)
    def test_property_3_add_existing_name_updates_config(
        self, name: str, url1: str, pat1: str, url2: str, pat2: str
    ):
        """
        **Feature: dynamic-jira-config, Property 3: Add existing name updates config**
        
        For any configuration name that already exists, calling add_config with that name
        should update the URL/PAT without increasing the total config count.
        
        **Validates: Requirements 2.2**
        """
        manager = ConfigManager(load_from_env=False)
        
        # Add initial config
        manager.add_config(name, url1, pat1)
        initial_count = manager.list_configs().data["count"]
        
        # Update with same name
        result = manager.add_config(name, url2, pat2)
        assert result.success
        assert result.data["action"] == "updated"
        
        # Count should remain the same
        final_count = manager.list_configs().data["count"]
        assert final_count == initial_count, "Config count changed after update"
        
        # URL should be updated
        profiles = manager.list_configs().data["profiles"]
        profile = next(p for p in profiles if p["name"] == name)
        assert profile["base_url"] == url2.rstrip("/")

    @settings(max_examples=100)
    @given(name=config_name_strategy, url=url_strategy, pat=pat_strategy)
    def test_property_4_list_configs_masks_pat(self, name: str, url: str, pat: str):
        """
        **Feature: dynamic-jira-config, Property 4: List configs masks PAT**
        
        For any stored configuration, list_configs should return the name and URL
        but never expose the full PAT value.
        
        **Validates: Requirements 2.3**
        """
        manager = ConfigManager(load_from_env=False)
        manager.add_config(name, url, pat)
        
        list_result = manager.list_configs()
        profiles = list_result.data["profiles"]
        profile = next(p for p in profiles if p["name"] == name)
        
        # PAT should be masked (contain ****)
        assert "****" in profile["pat"], "PAT is not masked"
        # Full PAT should not be exposed
        if len(pat) > 8:
            assert profile["pat"] != pat, "Full PAT is exposed"

    @settings(max_examples=100)
    @given(name=config_name_strategy, url=url_strategy, pat=pat_strategy)
    def test_property_5_remove_config_removes_from_list(self, name: str, url: str, pat: str):
        """
        **Feature: dynamic-jira-config, Property 5: Remove config removes from list**
        
        For any stored configuration name, after calling remove_config,
        that name should no longer appear in list_configs.
        
        **Validates: Requirements 2.4**
        """
        manager = ConfigManager(load_from_env=False)
        
        # Add then remove
        manager.add_config(name, url, pat)
        result = manager.remove_config(name)
        assert result.success
        
        # Should not appear in list
        list_result = manager.list_configs()
        names = [p["name"] for p in list_result.data["profiles"]]
        assert name not in names, f"Config '{name}' still in list after removal"

    @settings(max_examples=100)
    @given(name=config_name_strategy, url=url_strategy, pat=pat_strategy)
    def test_property_6_remove_active_config_clears_active(self, name: str, url: str, pat: str):
        """
        **Feature: dynamic-jira-config, Property 6: Remove active config clears active**
        
        For any configuration that is currently active, removing it should result
        in no active configuration.
        
        **Validates: Requirements 2.5**
        """
        manager = ConfigManager(load_from_env=False)
        
        # Add and set as active
        manager.add_config(name, url, pat)
        manager.set_active(name)
        
        # Verify it's active
        assert manager.get_active().data["active"] is not None
        
        # Remove it
        result = manager.remove_config(name)
        assert result.success
        
        # Active should be cleared
        active_result = manager.get_active()
        assert active_result.data["active"] is None, "Active config not cleared after removal"

    @settings(max_examples=100)
    @given(name=config_name_strategy, url=url_strategy, pat=pat_strategy)
    def test_property_7_set_active_with_valid_name_succeeds(self, name: str, url: str, pat: str):
        """
        **Feature: dynamic-jira-config, Property 7: Set active with valid name succeeds**
        
        For any configuration name that exists in the stored profiles,
        set_active should succeed and get_active should return that name.
        
        **Validates: Requirements 3.1, 3.3**
        """
        manager = ConfigManager(load_from_env=False)
        
        # Add config
        manager.add_config(name, url, pat)
        
        # Set active
        result = manager.set_active(name)
        assert result.success, f"set_active failed: {result.error}"
        
        # Verify get_active returns correct name
        active_result = manager.get_active()
        assert active_result.data["active"]["name"] == name

    @settings(max_examples=100)
    @given(
        existing_name=config_name_strategy,
        invalid_name=config_name_strategy,
        url=url_strategy,
        pat=pat_strategy
    )
    def test_property_8_set_active_with_invalid_name_fails(
        self, existing_name: str, invalid_name: str, url: str, pat: str
    ):
        """
        **Feature: dynamic-jira-config, Property 8: Set active with invalid name fails**
        
        For any name that does not exist in stored profiles,
        set_active should return an error.
        
        **Validates: Requirements 3.2**
        """
        # Ensure names are different
        if existing_name == invalid_name:
            invalid_name = invalid_name + "_invalid"
        
        manager = ConfigManager(load_from_env=False)
        manager.add_config(existing_name, url, pat)
        
        # Try to set invalid name as active
        result = manager.set_active(invalid_name)
        assert not result.success, "set_active should fail for invalid name"
        assert "not found" in result.error.lower()


class TestConfigManagerEdgeCases:
    """Unit tests for edge cases."""

    def test_empty_manager_list(self):
        """Empty manager should return empty list."""
        manager = ConfigManager(load_from_env=False)
        result = manager.list_configs()
        assert result.success
        assert result.data["count"] == 0
        assert result.data["profiles"] == []

    def test_empty_name_rejected(self):
        """Empty name should be rejected."""
        manager = ConfigManager(load_from_env=False)
        result = manager.add_config("", "https://example.com", "token123")
        assert not result.success
        assert "name" in result.error.lower()

    def test_empty_url_rejected(self):
        """Empty URL should be rejected."""
        manager = ConfigManager(load_from_env=False)
        result = manager.add_config("test", "", "token123")
        assert not result.success
        assert "url" in result.error.lower()

    def test_empty_pat_rejected(self):
        """Empty PAT should be rejected."""
        manager = ConfigManager(load_from_env=False)
        result = manager.add_config("test", "https://example.com", "")
        assert not result.success
        assert "pat" in result.error.lower()

    def test_remove_nonexistent_config(self):
        """Removing non-existent config should fail."""
        manager = ConfigManager(load_from_env=False)
        result = manager.remove_config("nonexistent")
        assert not result.success
        assert "not found" in result.error.lower()

    def test_get_active_config_returns_profile(self):
        """get_active_config should return JiraConfigProfile object."""
        manager = ConfigManager(load_from_env=False)
        manager.add_config("test", "https://example.com", "token123")
        manager.set_active("test")
        
        config = manager.get_active_config()
        assert config is not None
        assert isinstance(config, JiraConfigProfile)
        assert config.name == "test"
        assert config.base_url == "https://example.com"
        assert config.pat == "token123"

    def test_get_active_config_returns_none_when_no_active(self):
        """get_active_config should return None when no active config."""
        manager = ConfigManager(load_from_env=False)
        assert manager.get_active_config() is None

    def test_url_trailing_slash_removed(self):
        """Trailing slash should be removed from URLs."""
        manager = ConfigManager(load_from_env=False)
        manager.add_config("test", "https://example.com/", "token123")
        
        config = manager.get_active_config()
        manager.set_active("test")
        config = manager.get_active_config()
        assert config.base_url == "https://example.com"


class TestMultiInstanceEnvVarLoading:
    """Tests for loading multiple Jira instances from environment variables."""

    def test_multi_instance_env_vars_loaded(self):
        """
        JIRA_<NAME>_URL and JIRA_<NAME>_PAT patterns should create named profiles.
        """
        # Set up environment variables
        original_env = os.environ.copy()
        try:
            os.environ["JIRA_LOCAL_URL"] = "https://local.jira.com"
            os.environ["JIRA_LOCAL_PAT"] = "local-token-123"
            os.environ["JIRA_VM_URL"] = "https://vm.jira.com"
            os.environ["JIRA_VM_PAT"] = "vm-token-456"
            
            manager = ConfigManager(load_from_env=True)
            
            # Should have both profiles
            result = manager.list_configs()
            assert result.success
            names = [p["name"] for p in result.data["profiles"]]
            assert "local" in names, "local profile not loaded"
            assert "vm" in names, "vm profile not loaded"
            
            # Verify URLs
            profiles = {p["name"]: p for p in result.data["profiles"]}
            assert profiles["local"]["base_url"] == "https://local.jira.com"
            assert profiles["vm"]["base_url"] == "https://vm.jira.com"
            
        finally:
            # Restore original environment
            os.environ.clear()
            os.environ.update(original_env)

    def test_legacy_and_multi_instance_coexist(self):
        """
        Legacy JIRA_BASE_URL/JIRA_PAT and multi-instance patterns should coexist.
        """
        original_env = os.environ.copy()
        try:
            # Legacy config
            os.environ["JIRA_BASE_URL"] = "https://default.jira.com"
            os.environ["JIRA_PAT"] = "default-token"
            # Multi-instance config
            os.environ["JIRA_PROD_URL"] = "https://prod.jira.com"
            os.environ["JIRA_PROD_PAT"] = "prod-token"
            
            manager = ConfigManager(load_from_env=True)
            
            result = manager.list_configs()
            names = [p["name"] for p in result.data["profiles"]]
            assert "default" in names, "default profile not loaded"
            assert "prod" in names, "prod profile not loaded"
            
        finally:
            os.environ.clear()
            os.environ.update(original_env)

    def test_incomplete_multi_instance_ignored(self):
        """
        If only URL or only PAT is provided for a name, it should be ignored.
        """
        original_env = os.environ.copy()
        try:
            # Only URL, no PAT
            os.environ["JIRA_INCOMPLETE_URL"] = "https://incomplete.jira.com"
            # Complete config
            os.environ["JIRA_COMPLETE_URL"] = "https://complete.jira.com"
            os.environ["JIRA_COMPLETE_PAT"] = "complete-token"
            
            manager = ConfigManager(load_from_env=True)
            
            result = manager.list_configs()
            names = [p["name"] for p in result.data["profiles"]]
            assert "incomplete" not in names, "incomplete profile should not be loaded"
            assert "complete" in names, "complete profile should be loaded"
            
        finally:
            os.environ.clear()
            os.environ.update(original_env)

    def test_first_profile_set_as_active(self):
        """
        The first loaded profile should be set as active automatically.
        """
        original_env = os.environ.copy()
        try:
            # Clear any existing JIRA env vars first
            keys_to_remove = [k for k in os.environ if k.startswith("JIRA_")]
            for k in keys_to_remove:
                del os.environ[k]
            
            os.environ["JIRA_TEST_URL"] = "https://test.jira.com"
            os.environ["JIRA_TEST_PAT"] = "test-token"
            
            manager = ConfigManager(load_from_env=True)
            
            # Should have an active config
            result = manager.get_active()
            assert result.data["active"] is not None
            assert result.data["active"]["name"] == "test"
            
        finally:
            os.environ.clear()
            os.environ.update(original_env)



class TestToolsRequireActiveConfig:
    """
    **Feature: dynamic-jira-config, Property 1: Tools require active configuration**
    
    Tests that Jira tools return appropriate errors when no active config exists.
    **Validates: Requirements 1.3, 5.2**
    """

    def test_get_jira_client_raises_without_active_config(self):
        """get_jira_client should raise ValueError when no active config."""
        import sys
        sys.path.insert(0, str(__file__).replace("tests/test_config_manager.py", "src"))
        
        # Import server module and reset the global config manager
        import importlib
        import server
        
        # Create a fresh ConfigManager with no env vars
        original_manager = server._config_manager
        server._config_manager = ConfigManager(load_from_env=False)
        
        try:
            # Should raise ValueError
            with pytest.raises(ValueError) as exc_info:
                server.get_jira_client()
            
            assert "No active Jira configuration" in str(exc_info.value)
        finally:
            # Restore original manager
            server._config_manager = original_manager

    def test_helper_returns_error_dict_without_active_config(self):
        """_get_client_or_error should return error dict when no active config."""
        import sys
        sys.path.insert(0, str(__file__).replace("tests/test_config_manager.py", "src"))
        
        import server
        
        # Create a fresh ConfigManager with no env vars
        original_manager = server._config_manager
        server._config_manager = ConfigManager(load_from_env=False)
        
        try:
            result = server._get_client_or_error()
            
            # Should return error dict, not JiraClient
            assert isinstance(result, dict)
            assert result["success"] is False
            assert "No active Jira configuration" in result["error"]
        finally:
            # Restore original manager
            server._config_manager = original_manager
