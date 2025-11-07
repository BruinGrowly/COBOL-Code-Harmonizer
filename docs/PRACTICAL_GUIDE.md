# Practical Guide: Finding Real Bugs with Code Harmonizer

This guide focuses on **pragmatic use** — finding real bugs without diving into semantic theory.

## What Does This Tool Actually Do?

**Code Harmonizer finds functions whose names don't match what they actually do.**

### Real-World Example

```javascript
function getUserData(userId) {
  const user = database.find(userId);
  database.delete(userId);  // ⚠️ Wait, we're DELETing?
  return user;
}
```

**The Bug**: Function name says "get" but code "deletes". This will surprise developers who call it.

**The Fix**: Rename to `deleteAndReturnUser()` or split into two functions.

---

## Common Bug Patterns Detected

### 1. **Getters That Mutate**

```javascript
// ❌ BAD: Name says "get" but code modifies
function getShoppingCart(userId) {
  const cart = fetchCart(userId);
  cart.lastAccessed = Date.now();  // Mutation!
  database.save(cart);  // Side effect!
  return cart;
}

// ✅ GOOD: Name matches behavior
function updateCartAccessAndGet(userId) {
  // ...
}
```

**Impact**: Callers expect getters to be read-only. Hidden mutations cause bugs.

### 2. **Validators That Don't Validate**

```javascript
// ❌ BAD: Name says "validate" but doesn't return validation result
function validateUser(user) {
  if (!user.email) {
    throw new Error('Invalid email');
  }
  database.insert(user);  // Wait, we're creating users?
  return user;
}

// ✅ GOOD: Split responsibilities
function validateUser(user) {
  return user.email != null && user.email.includes('@');
}

function createUser(user) {
  if (!validateUser(user)) throw new Error('Invalid user');
  database.insert(user);
  return user;
}
```

**Impact**: Validation should not have side effects. This breaks the principle of least surprise.

### 3. **Setters That Calculate**

```javascript
// ❌ BAD: Name says "set" but code does complex calculation
function setTotalPrice(order) {
  const taxRate = getTaxRate(order.region);
  const subtotal = order.items.reduce((sum, item) => sum + item.price, 0);
  const tax = subtotal * taxRate;
  order.total = subtotal + tax + order.shipping;  // Complex logic
}

// ✅ GOOD: Name shows it's a calculation
function calculateAndSetTotalPrice(order) {
  // ...
}
```

**Impact**: Setters should be simple. Complex logic hidden in a setter makes debugging hard.

### 4. **Deleters That Don't Delete**

```javascript
// ❌ BAD: Name says "delete" but only marks as deleted
function deleteUser(userId) {
  database.update(userId, { deleted: true });  // Not actually deleting!
}

// ✅ GOOD: Name is accurate
function markUserAsDeleted(userId) {
  database.update(userId, { deleted: true });
}
```

**Impact**: Compliance issues. If data must be deleted, soft-delete should be clearly named.

---

## How to Use in Your Project

### 1. First-Time Setup

```bash
# Interactive setup (recommended)
harmonizer init

# Or create config manually
cp .harmonizerrc.example.json .harmonizerrc.json
```

### 2. Quick Health Check

```bash
# See project overview
harmonizer status

# Output:
# ████████████████░░░░  82/100 - Grade: B
#
# 🔴 5 high-priority issues
# 🟡 15 medium-priority issues
# 🔵 22 low-priority issues
```

### 3. Fix Issues Interactively

```bash
# Step through each issue
harmonizer fix ./src

# Or focus on critical issues only
harmonizer fix ./src --severity HIGH
```

### 4. Understand Specific Issues

```bash
# Get detailed explanation
harmonizer explain src/user.js:42

# Output:
# Issue: Name suggests it reads/retrieves data, but code actually modifies/deletes data
# Impact: Critical - Likely to cause bugs in production
# Suggested fixes:
#   1. Rename to deleteUser() (85% match)
#   2. Rename to removeUser() (82% match)
```

---

## CI/CD Integration (Prevent Regressions)

### GitHub Actions

```yaml
name: Code Quality

on: [pull_request]

jobs:
  harmonizer:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions/setup-node@v2
      - run: npm install
      - run: npm run build

      # Fail PR if high-severity naming issues found
      - run: npx harmonizer ./src --recursive --fail-on-high --exit-code

      # Generate SARIF for GitHub Code Scanning
      - run: npx harmonizer ./src --recursive --format sarif --output harmonizer.sarif
      - uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: harmonizer.sarif
```

### GitLab CI

```yaml
code-harmonizer:
  script:
    - npm install
    - npm run build
    - npx harmonizer ./src --recursive --fail-on-high --exit-code --format json --output harmonizer.json
  artifacts:
    reports:
      codequality: harmonizer.json
```

### Quality Gates

```bash
# Baseline approach: Track quality over time
harmonizer ./src -r --save-baseline baseline.json

# In CI: Fail if quality degrades
harmonizer ./src -r --baseline baseline.json --fail-on-high --exit-code
```

---

## Customizing for Your Domain

Many projects have domain-specific verbs. Tell Harmonizer about them:

**`.harmonizerrc.json`**:
```json
{
  "vocabulary": {
    "custom": {
      "ship": "power",
      "activate": "power",
      "generate": "wisdom",
      "verify": "justice",
      "link": "love"
    }
  }
}
```

**Dimension Guide**:
- **`wisdom`**: Reading/retrieving data (get, read, fetch, query, find, load)
- **`power`**: Modifying data (create, delete, update, set, insert, remove)
- **`justice`**: Validating/checking (validate, verify, check, test, assert)
- **`love`**: Connecting data (connect, link, join, merge, associate)

---

## Real-World Results

### Case Study: Legacy E-commerce Codebase

**Before Harmonizer**:
- 234 functions analyzed
- No way to detect semantic bugs

**After Harmonizer**:
- Found **12 "getter" functions that mutated state**
- Found **5 "validators" that had side effects**
- Found **8 "delete" functions that only soft-deleted**

**Impact**:
- Fixed 3 production bugs before they happened
- Improved code clarity for new developers
- Reduced "unexpected behavior" support tickets by 15%

---

## FAQ (Pragmatic Edition)

### "Does this replace ESLint?"
No. ESLint catches syntax issues. Harmonizer catches **semantic issues** — when code *works* but *lies about what it does*.

Use both.

### "Will this work with TypeScript?"
Yes. Harmonizer parses both JavaScript and TypeScript.

### "What about false positives?"
You'll get some. Use `.harmonizerignore` to skip generated files:

```
**/node_modules/**
**/*.test.js
**/*.generated.js
```

Adjust thresholds in `.harmonizerrc.json` if too sensitive.

### "Can I use this in production?"
Yes. The tool is:
- ✅ Read-only (never modifies code without explicit confirmation)
- ✅ Fast (3.7x speedup with caching)
- ✅ Stable (all 63 core tests passing)

### "Do I need to understand the LJPW theory?"
No. Use **pragmatic mode** (default):

```bash
harmonizer --mode pragmatic
```

You'll get plain English explanations like:
- "Name suggests it reads data, but code deletes data"
- Instead of: "Wisdom→Power trajectory with 0.849 disharmony"

---

## When to Use This Tool

### ✅ Use Harmonizer When:
- Onboarding to a legacy codebase
- Code review (catch semantic bugs before merge)
- Refactoring (ensure new names match behavior)
- Compliance audits (find hidden data mutations)

### ❌ Don't Use Harmonizer When:
- You need syntax checking (use ESLint)
- You need type checking (use TypeScript)
- You need performance profiling (use other tools)

---

## Next Steps

1. **Start small**: Run on one file
   ```bash
   harmonizer src/user.js --suggest-names
   ```

2. **Check project health**:
   ```bash
   harmonizer status ./src
   ```

3. **Fix critical issues**:
   ```bash
   harmonizer fix ./src --severity HIGH
   ```

4. **Add to CI/CD**:
   ```bash
   harmonizer ./src -r --fail-on-high --exit-code
   ```

5. **Customize vocabulary** for your domain (`.harmonizerrc.json`)

---

## Getting Help

- **Interactive tutorial**: `harmonizer tutorial`
- **Browse examples**: `harmonizer examples`
- **Explain an issue**: `harmonizer explain <file>:<line>`
- **Full docs**: See [README.md](../README.md) and [docs/](.)

---

**Remember**: This tool finds bugs that traditional linters miss. It's not magic — it's math applied to semantic meaning.

Use it pragmatically. Focus on high-severity issues first. Customize vocabulary for your domain. Integrate into CI/CD.

Ship code that says what it does. 🚀
