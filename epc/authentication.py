#!/usr/bin/env python3
"""
Authentication System for Wolfram EPC

Self-provisioning user authentication with subdomain-based restrictions.
Integrates with WolfCog's security model.
"""

import hashlib
import secrets
import time
from pathlib import Path
from typing import Dict, List, Optional, Set
from dataclasses import dataclass, field
import json


@dataclass
class User:
    """User account information"""
    username: str
    email: str
    password_hash: str
    subdomains: Set[str] = field(default_factory=set)
    roles: Set[str] = field(default_factory=set)
    api_key: str = ""
    created_at: float = 0.0
    last_login: Optional[float] = None
    is_active: bool = True


class AuthenticationSystem:
    """
    EPC Authentication and User Provisioning System
    
    Features:
    - Self-provisioning user accounts
    - Subdomain-based access restrictions
    - API key management
    - Role-based access control
    - Session management
    """
    
    def __init__(self, data_dir: Optional[Path] = None):
        """
        Initialize authentication system
        
        Args:
            data_dir: Directory for storing user data
        """
        self.data_dir = data_dir or Path("/tmp/epc_auth")
        self.data_dir.mkdir(exist_ok=True, parents=True)
        
        self.users: Dict[str, User] = {}
        self.sessions: Dict[str, Dict] = {}  # session_token -> session_info
        self.api_keys: Dict[str, str] = {}  # api_key -> username
        
        # Load existing users
        self._load_users()
        
        # Default roles
        self.roles = {
            "admin": {"manage_users", "deploy_apis", "access_all_subdomains"},
            "developer": {"deploy_apis", "access_assigned_subdomains"},
            "user": {"access_assigned_subdomains"}
        }
    
    def _load_users(self):
        """Load users from storage"""
        users_file = self.data_dir / "users.json"
        if users_file.exists():
            try:
                with open(users_file) as f:
                    data = json.load(f)
                    for username, user_data in data.items():
                        self.users[username] = User(
                            username=user_data["username"],
                            email=user_data["email"],
                            password_hash=user_data["password_hash"],
                            subdomains=set(user_data.get("subdomains", [])),
                            roles=set(user_data.get("roles", ["user"])),
                            api_key=user_data.get("api_key", ""),
                            created_at=user_data.get("created_at", time.time()),
                            last_login=user_data.get("last_login"),
                            is_active=user_data.get("is_active", True)
                        )
                        if user_data.get("api_key"):
                            self.api_keys[user_data["api_key"]] = username
            except Exception as e:
                print(f"Warning: Could not load users: {e}")
    
    def _save_users(self):
        """Save users to storage"""
        users_file = self.data_dir / "users.json"
        try:
            data = {
                username: {
                    "username": user.username,
                    "email": user.email,
                    "password_hash": user.password_hash,
                    "subdomains": list(user.subdomains),
                    "roles": list(user.roles),
                    "api_key": user.api_key,
                    "created_at": user.created_at,
                    "last_login": user.last_login,
                    "is_active": user.is_active
                }
                for username, user in self.users.items()
            }
            with open(users_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            print(f"Warning: Could not save users: {e}")
    
    def register_user(self, username: str, email: str, password: str,
                     subdomains: Optional[List[str]] = None,
                     roles: Optional[List[str]] = None) -> User:
        """
        Self-provision a new user account
        
        Args:
            username: Unique username
            email: User email
            password: User password
            subdomains: Accessible subdomains
            roles: User roles
            
        Returns:
            Created User object
        """
        if username in self.users:
            raise ValueError(f"Username '{username}' already exists")
        
        # Hash password
        password_hash = self._hash_password(password)
        
        # Generate API key
        api_key = self._generate_api_key()
        
        # Create user
        user = User(
            username=username,
            email=email,
            password_hash=password_hash,
            subdomains=set(subdomains or []),
            roles=set(roles or ["user"]),
            api_key=api_key,
            created_at=time.time()
        )
        
        self.users[username] = user
        self.api_keys[api_key] = username
        self._save_users()
        
        return user
    
    def authenticate(self, username: str, password: str) -> Optional[str]:
        """
        Authenticate user and create session
        
        Args:
            username: Username
            password: Password
            
        Returns:
            Session token if successful, None otherwise
        """
        if username not in self.users:
            return None
        
        user = self.users[username]
        
        if not user.is_active:
            return None
        
        # Verify password
        if not self._verify_password(password, user.password_hash):
            return None
        
        # Create session
        session_token = secrets.token_urlsafe(32)
        self.sessions[session_token] = {
            "username": username,
            "created_at": time.time(),
            "last_activity": time.time()
        }
        
        # Update last login
        user.last_login = time.time()
        self._save_users()
        
        return session_token
    
    def authenticate_api_key(self, api_key: str) -> Optional[str]:
        """
        Authenticate using API key
        
        Args:
            api_key: API key
            
        Returns:
            Username if valid, None otherwise
        """
        return self.api_keys.get(api_key)
    
    def validate_session(self, session_token: str) -> Optional[str]:
        """
        Validate session token
        
        Args:
            session_token: Session token
            
        Returns:
            Username if valid, None otherwise
        """
        if session_token not in self.sessions:
            return None
        
        session = self.sessions[session_token]
        
        # Check if session expired (24 hour timeout)
        if time.time() - session["last_activity"] > 86400:
            del self.sessions[session_token]
            return None
        
        # Update activity
        session["last_activity"] = time.time()
        
        return session["username"]
    
    def check_subdomain_access(self, username: str, subdomain: str) -> bool:
        """
        Check if user has access to subdomain
        
        Args:
            username: Username
            subdomain: Subdomain to check
            
        Returns:
            True if user has access
        """
        if username not in self.users:
            return False
        
        user = self.users[username]
        
        # Admins have access to all subdomains
        if "admin" in user.roles:
            return True
        
        # Check if user has access to this subdomain
        return subdomain in user.subdomains
    
    def grant_subdomain_access(self, username: str, subdomain: str):
        """Grant user access to subdomain"""
        if username not in self.users:
            raise ValueError(f"Unknown user: {username}")
        
        user = self.users[username]
        user.subdomains.add(subdomain)
        self._save_users()
    
    def revoke_subdomain_access(self, username: str, subdomain: str):
        """Revoke user access to subdomain"""
        if username not in self.users:
            raise ValueError(f"Unknown user: {username}")
        
        user = self.users[username]
        user.subdomains.discard(subdomain)
        self._save_users()
    
    def _hash_password(self, password: str) -> str:
        """Hash password using SHA-256"""
        salt = secrets.token_hex(16)
        pwd_hash = hashlib.sha256((password + salt).encode()).hexdigest()
        return f"{salt}${pwd_hash}"
    
    def _verify_password(self, password: str, password_hash: str) -> bool:
        """Verify password against hash"""
        try:
            salt, pwd_hash = password_hash.split('$')
            check_hash = hashlib.sha256((password + salt).encode()).hexdigest()
            return check_hash == pwd_hash
        except:
            return False
    
    def _generate_api_key(self) -> str:
        """Generate unique API key"""
        while True:
            api_key = f"epc_{secrets.token_urlsafe(32)}"
            if api_key not in self.api_keys:
                return api_key
    
    def get_user_info(self, username: str) -> Optional[Dict]:
        """Get user information (excluding sensitive data)"""
        if username not in self.users:
            return None
        
        user = self.users[username]
        return {
            "username": user.username,
            "email": user.email,
            "subdomains": list(user.subdomains),
            "roles": list(user.roles),
            "created_at": user.created_at,
            "last_login": user.last_login,
            "is_active": user.is_active
        }
    
    def list_users(self) -> List[Dict]:
        """List all users (admin only)"""
        return [self.get_user_info(username) for username in self.users.keys()]
