// Code under test: it imports the mocked module itself, so the tests can prove
// that the consumer and the test file observe the very same mock instance.
import { add, label } from "./mock-target.js";

export const readLabel = () => label;
export const callAdd = (a, b) => add(a, b);
