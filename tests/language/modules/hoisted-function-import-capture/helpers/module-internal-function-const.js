const describeRead = (read) => {
  try {
    return String(read());
  } catch (error) {
    return error.name;
  }
};

export const internalBeforeInitialization = describeRead(readInternal);
export const exportedBeforeInitialization = describeRead(readExported);

const LIMIT = 16;

function readInternal() {
  return LIMIT;
}

export function readExported() {
  return LIMIT;
}

export const internalAfterInitialization = describeRead(readInternal);
export const exportedAfterInitialization = describeRead(readExported);
