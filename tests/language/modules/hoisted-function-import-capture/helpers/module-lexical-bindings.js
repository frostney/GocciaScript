const constValue = "const-ready";
let letValue = "let-ready";
let reassignedLetValue;

reassignedLetValue = "let-reassigned";

export function readConst() {
  return constValue;
}

export function readLet() {
  return letValue;
}

export function readReassignedLet() {
  return reassignedLetValue;
}

export function setLet(value) {
  letValue = value;
}

function readViaExportList() {
  return `${constValue}:${letValue}`;
}

export { readViaExportList };

export const arrowReadsConst = () => constValue;
export const expressionReadsLet = function () {
  return letValue;
};

export default (function () {
  return constValue;
});
