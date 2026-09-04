const { test } = require('node:test');
const assert = require('node:assert/strict');
const { mkdtempSync, writeFileSync, readFileSync, rmSync } = require('node:fs');
const { tmpdir } = require('node:os');
const { join, resolve } = require('node:path');
const { spawnSync } = require('node:child_process');
const upsert = require('./upsert-pr-comment.js');

test('report comments find current and legacy markers on later pages', async () => {
  for (const previous of [null, '<!-- current --> old', '<!-- legacy --> old']) {
    const calls = [];
    const github = { rest: { issues: {
      listComments: async ({ page = 1, ...issue }) => {
        assert.deepEqual(issue, { owner: 'owner', repo: 'repo', issue_number: 7 });
        return { data: page === 1 ? [{ id: 1, body: 'unrelated' }] : [{ id: 2, body: previous }] };
      },
      createComment: async args => calls.push(['create', args]),
      updateComment: async args => calls.push(['update', args]),
    } }, paginate: async (method, issue) => {
      assert.equal(method, github.rest.issues.listComments);
      const first = await method(issue);
      const second = await method({ ...issue, page: 2 });
      return [...first.data, ...second.data];
    } };
    await upsert({ github, context: { repo: { owner: 'owner', repo: 'repo' }, issue: { number: 7 } },
      body: '<!-- current --> new', markers: ['<!-- current -->', '<!-- legacy -->'] });
    assert.deepEqual(calls, [[previous ? 'update' : 'create', {
      owner: 'owner', repo: 'repo', ...(previous ? { comment_id: 2 } : { issue_number: 7 }),
      body: '<!-- current --> new',
    }]]);
  }
});

test('report launcher skips missing tokens, shares installation, and preserves failures and arguments', () => {
  const dir = mkdtempSync(join(tmpdir(), 'goccia-publish-'));
  try {
    const log = join(dir, 'calls.jsonl');
    writeFileSync(join(dir, 'bun'), `#!/usr/bin/env node
require('node:fs').appendFileSync(process.env.PUBLISH_TEST_LOG, JSON.stringify({ args: process.argv.slice(2), cwd: process.cwd() }) + '\\n');
process.exit(Number(process.argv[2] === 'install' ? process.env.INSTALL_EXIT : process.env.PUBLISH_EXIT) || 0);
`, { mode: 0o755 });
    const env = { ...process.env, PATH: `${dir}:${process.env.PATH}`, RUNNER_TEMP: dir,
      PUBLISH_TEST_LOG: log, BLOB_READ_WRITE_TOKEN: '', INSTALL_EXIT: '0', PUBLISH_EXIT: '0' };
    const run = () => spawnSync('bash', [resolve(__dirname, 'publish-website-report.sh'),
      'run', 'publish-awfy', '../report with spaces.json'], { env, encoding: 'utf8' });
    assert.equal(run().status, 0);
    assert.throws(() => readFileSync(log), { code: 'ENOENT' });
    env.BLOB_READ_WRITE_TOKEN = 'test-token';
    env.INSTALL_EXIT = '17';
    assert.equal(run().status, 17);
    env.INSTALL_EXIT = '0';
    assert.equal(run().status, 0);
    env.PUBLISH_EXIT = '23';
    assert.equal(run().status, 23);
    const calls = readFileSync(log, 'utf8').trim().split('\n').map(JSON.parse);
    assert.deepEqual(calls.map(c => c.args), [
      ['install', '--frozen-lockfile'], ['install', '--frozen-lockfile'],
      ['run', 'publish-awfy', '../report with spaces.json'],
      ['run', 'publish-awfy', '../report with spaces.json'],
    ]);
    assert.ok(calls.every(c => c.cwd === resolve(__dirname, '../../website')));
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
