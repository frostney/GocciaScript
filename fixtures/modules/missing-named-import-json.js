import { thisExportDoesNotExist } from "../../tests/language/modules/helpers/config.json" with { type: "json" };

export const value = thisExportDoesNotExist;
