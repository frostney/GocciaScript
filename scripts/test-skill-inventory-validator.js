#!/usr/bin/env node

const {
  mkdtempSync,
  mkdirSync,
  rmSync,
  writeFileSync,
} = require('node:fs');
const { tmpdir } = require('node:os');
const { join } = require('node:path');
const {
  computeSkillFolderHash,
  validateSkillInventory,
} = require('./validate-skill-inventory.js');

let passed = 0;

function assertThrows(callback, expected, message) {
  try {
    callback();
  } catch (error) {
    if (!error.message.includes(expected)) {
      throw new Error(`${message}: unexpected error: ${error.message}`);
    }
    passed += 1;
    return;
  }
  throw new Error(`${message}: expected an error containing ${expected}`);
}

const root = mkdtempSync(join(tmpdir(), 'skill-inventory-validator-'));
const managedDir = join(root, '.agents', 'skills', 'managed');
const inventoryPath = join(root, 'inventory.json');
const lockPath = join(root, 'skills-lock.json');

try {
  mkdirSync(join(managedDir, 'agents'), { recursive: true });
  writeFileSync(join(managedDir, 'SKILL.md'), '---\nname: managed\n---\n');
  writeFileSync(join(managedDir, 'agents', 'openai.yaml'), 'name: managed\n');

  const writeInventory = (items) => {
    writeFileSync(inventoryPath, `${JSON.stringify(items, null, 2)}\n`);
  };
  const writeLock = (hash) => {
    writeFileSync(lockPath, `${JSON.stringify({
      version: 1,
      skills: {
        managed: {
          source: 'example/skills',
          sourceType: 'github',
          skillPath: 'managed/SKILL.md',
          computedHash: hash,
        },
      },
    }, null, 2)}\n`);
  };

  const expectedInventory = [
    { name: 'managed', path: managedDir, scope: 'project', agents: ['Codex'] },
    { name: 'repo-native', path: 'unused', scope: 'project', agents: ['Codex'] },
  ];
  const originalHash = computeSkillFolderHash(managedDir);
  writeLock(originalHash);
  writeInventory(expectedInventory);

  if (validateSkillInventory(inventoryPath, { root, lockPath }) !== 1) {
    throw new Error('valid inventory did not report one managed skill');
  }
  passed += 1;

  writeFileSync(join(managedDir, 'SKILL.md'), 'changed\n');
  assertThrows(
    () => validateSkillInventory(inventoryPath, { root, lockPath }),
    'does not match lock',
    'content drift is rejected',
  );

  writeFileSync(join(managedDir, 'SKILL.md'), '---\nname: managed\n---\n');
  writeInventory(expectedInventory.slice(1));
  assertThrows(
    () => validateSkillInventory(inventoryPath, { root, lockPath }),
    'lock-managed skill managed is missing',
    'missing inventory entries are rejected',
  );

  writeInventory([{ ...expectedInventory[0], path: join(root, 'stale') }]);
  assertThrows(
    () => validateSkillInventory(inventoryPath, { root, lockPath }),
    'unexpected canonical path',
    'stale inventory paths are rejected',
  );

  writeInventory([expectedInventory[0], expectedInventory[0]]);
  assertThrows(
    () => validateSkillInventory(inventoryPath, { root, lockPath }),
    'duplicate skill managed',
    'duplicate inventory entries are rejected',
  );

  writeInventory({ name: 'managed' });
  assertThrows(
    () => validateSkillInventory(inventoryPath, { root, lockPath }),
    'expected a JSON array',
    'non-array inventory is rejected',
  );
} finally {
  rmSync(root, { recursive: true, force: true });
}

console.log(`skill-inventory-validator: ${passed} assertions passed.`);
