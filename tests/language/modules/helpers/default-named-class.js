export default class NamedDefaultClass {
  static label() {
    return "named default class";
  }
}

export const localNamedDefaultClassType = typeof NamedDefaultClass;
export const localNamedDefaultClassName = NamedDefaultClass.name;
