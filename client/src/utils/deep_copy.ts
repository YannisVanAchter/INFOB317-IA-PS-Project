
/**
 *  Deep copy an object
 *  
 *  @param obj {T} Object to be copied
 *  @returns Copied object
 */
export function deepCopy<T>(obj: T): T {
    return JSON.parse(JSON.stringify(obj));
}