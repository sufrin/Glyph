#include <jni.h>
#import <Foundation/Foundation.h>
#import "MacFreeSpace.h"

// Helper to fetch a capacity by key
static jlong capacityForKey(jstring jpath, JNIEnv *env, NSString *key) {
    const char *path = (*env)->GetStringUTFChars(env, jpath, NULL);
    if (path == NULL) return -1;

    NSString *nsPath = [NSString stringWithUTF8String:path];
    NSURL *url = [NSURL fileURLWithPath:nsPath];
    NSNumber *capacity = nil;

    [url getResourceValue:&capacity forKey:key error:nil];

    (*env)->ReleaseStringUTFChars(env, jpath, path);

    if (capacity == nil) return -1;
    return (jlong)[capacity longLongValue];
}

JNIEXPORT jlong JNICALL
Java_MacFreeSpace_getImportantAvailableSpace(JNIEnv *env, jclass clazz, jstring jpath) {
    return capacityForKey(jpath, env, NSURLVolumeAvailableCapacityForImportantUsageKey);
}

JNIEXPORT jlong JNICALL
Java_MacFreeSpace_getOpportunisticAvailableSpace(JNIEnv *env, jclass clazz, jstring jpath) {
    return capacityForKey(jpath, env, NSURLVolumeAvailableCapacityForOpportunisticUsageKey);
}