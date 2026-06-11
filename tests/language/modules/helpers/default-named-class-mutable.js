export default class MutableNamedDefaultClass {}

MutableNamedDefaultClass = class ReassignedNamedDefaultClass {};

export const reassignedNamedDefaultClassName = MutableNamedDefaultClass.name;
