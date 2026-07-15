#!/usr/bin/env node

const { createHash } = require('node:crypto');
const { readFileSync, readdirSync } = require('node:fs');
const { join, relative, resolve } = require('node:path');

const SHA256_RE = /^[0-9a-f]{64}$/;

function readJson(filePath, label) {
  try {
    return JSON.parse(readFileSync(filePath, 'utf8'));
  } catch (error) {
    throw new Error(`${label} ${filePath} is not valid JSON: ${error.message}`);
  }
}

function collectSkillFiles(baseDir, currentDir, files) {
  const entries = readdirSync(currentDir, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = join(currentDir, entry.name);
    if (entry.isDirectory()) {
      if (entry.name === '.git' || entry.name === 'node_modules') continue;
      collectSkillFiles(baseDir, fullPath, files);
    } else if (entry.isFile()) {
      files.push({
        relativePath: relative(baseDir, fullPath).split('\\').join('/'),
        content: readFileSync(fullPath),
      });
    }
  }
}

function computeSkillFolderHash(skillDir) {
  const files = [];
  collectSkillFiles(skillDir, skillDir, files);
  files.sort((left, right) => left.relativePath.localeCompare(right.relativePath));

  const hash = createHash('sha256');
  for (const file of files) {
    hash.update(file.relativePath);
    hash.update(file.content);
  }
  return hash.digest('hex');
}

function validateSkillInventory(inventoryPath, options = {}) {
  const root = resolve(options.root || process.cwd());
  const lockPath = resolve(options.lockPath || join(root, 'skills-lock.json'));
  const inventory = readJson(inventoryPath, 'Inventory');
  const lock = readJson(lockPath, 'Lock file');

  if (!Array.isArray(inventory)) {
    throw new Error(`${inventoryPath}: expected a JSON array`);
  }
  if (lock.version !== 1 || !lock.skills || Array.isArray(lock.skills)) {
    throw new Error(`${lockPath}: expected version 1 with a skills object`);
  }

  const inventoryByName = new Map();
  for (const item of inventory) {
    if (!item || typeof item.name !== 'string' || item.scope !== 'project') {
      throw new Error(`${inventoryPath}: every entry must name a project skill`);
    }
    if (inventoryByName.has(item.name)) {
      throw new Error(`${inventoryPath}: duplicate skill ${item.name}`);
    }
    inventoryByName.set(item.name, item);
  }

  let validated = 0;
  for (const [name, entry] of Object.entries(lock.skills)) {
    if (!entry || typeof entry.computedHash !== 'string' ||
      !SHA256_RE.test(entry.computedHash)) {
      throw new Error(`${lockPath}: ${name} has an invalid computedHash`);
    }
    if (!/^[a-z0-9][a-z0-9._-]*$/i.test(name)) {
      throw new Error(`${lockPath}: unsafe skill name ${name}`);
    }

    const expectedDir = resolve(root, '.agents', 'skills', name);
    const item = inventoryByName.get(name);
    if (!item) {
      throw new Error(`${inventoryPath}: lock-managed skill ${name} is missing`);
    }
    if (typeof item.path !== 'string' || resolve(item.path) !== expectedDir) {
      throw new Error(`${inventoryPath}: ${name} has an unexpected canonical path`);
    }

    const actualHash = computeSkillFolderHash(expectedDir);
    if (actualHash !== entry.computedHash) {
      throw new Error(
        `${name}: content hash ${actualHash} does not match lock ${entry.computedHash}`,
      );
    }
    validated += 1;
  }

  console.log(
    `Validated ${validated} lock-managed skills against inventory and content hashes.`,
  );
  return validated;
}

if (require.main === module) {
  const [inventoryPath] = process.argv.slice(2);
  if (!inventoryPath) {
    console.error('Usage: node scripts/validate-skill-inventory.js <inventory-json>');
    process.exit(2);
  }

  try {
    validateSkillInventory(inventoryPath);
  } catch (error) {
    console.error(error.message);
    process.exit(1);
  }
}

module.exports = { computeSkillFolderHash, validateSkillInventory };
