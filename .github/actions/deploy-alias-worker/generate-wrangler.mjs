#!/usr/bin/env node
import { readFileSync, writeFileSync } from "node:fs";

const jsonPath = process.argv[2];
const hostname = process.argv[3];
const zoneName = process.argv[4];
const workerName = process.argv[5];
const accountId = process.argv[6];
const outPath = process.argv[7];

const data = JSON.parse(readFileSync(jsonPath, "utf8"));
const aliases = data.aliases ?? [];
const patterns = [...new Set(aliases.map((e) => {
  const prefix = (e.from ?? []).join("/");
  if (!prefix) return null;
  return `${hostname}/${prefix}*`;
}).filter(Boolean))].sort();

const routes = patterns.map((pattern) =>
  `[[routes]]\npattern = "${pattern}"\nzone_name = "${zoneName}"`
).join("\n\n");

const toml = `name = "${workerName}"
main = "worker.js"
compatibility_date = "2026-08-28"
account_id = "${accountId}"
workers_dev = false

${routes}
`;

writeFileSync(outPath, toml);
console.log(`wrote ${patterns.length} Worker routes to ${outPath}`);
