export { name, version } from "./config.json";
export {
  name as attributedName,
  version as attributedVersion,
} from "./config.json" with { type: "json" };
export {
  default as configTextDefault,
} from "./config.json" with { type: "text" };
export * as configTextModule from "./config.json" with { type: "text" };
