
/**
 *  Deep copy an object
 *  
 *  @param obj {T} Object to be copied
 *  @returns Copied object
 */
export function deepCopy<T>(obj: T): T {
    const str = JSON.stringify(obj);
    if (str)
        return JSON.parse(str);
    return new Object() as T;
}