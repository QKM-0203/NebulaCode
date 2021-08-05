/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import java.util.List;

/**
 * create tag object or edgeType object.
 *
 * <p>{@link #name} is tagName or edgeTypeName,{@link #propertyList} is attribute list,
 * you can also set a specific expiration timestamp for a property.</p>
 *
 * @author Qi Kai Meng
 */
public class Schema {
    private final String name;
    private List<Property> propertyList;
    private long ttlDuration = 0;
    private String ttlCol;

    /**
     * create an Tag or EdgeType of include property.
     *
     * @param name         tag name or edgeName
     * @param propertyList Property List
     */
    public Schema(String name, List<Property> propertyList) {
        this.name = name;
        this.propertyList = propertyList;
    }

    /**
     * create an Tag or EdgeType of include property and property time setting.
     *
     * @param name         Tag name or EdgeType name
     * @param propertyList property List
     * @param ttlDuration  timestamp difference,timestamp difference default value is zero
     *                     symbol never expire
     * @param ttlCol       to determine which attribute to set the lifetime,
     *                     the type must be int or timestamp,if pass in null, symbol not set
     */
    public Schema(String name, List<Property> propertyList,
                  long ttlDuration, String ttlCol) {
        this.name = name;
        this.propertyList = propertyList;
        this.ttlDuration = ttlDuration;
        this.ttlCol = ttlCol;
    }

    /**
     * create an empty property.
     *
     * @param name Tag name or EdgeName
     */
    public Schema(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public List<Property> getPropertyList() {
        return propertyList;
    }

    public void setPropertyList(List<Property> propertyList) {
        this.propertyList = propertyList;
    }

    public long getTtlDuration() {
        return ttlDuration;
    }

    public void setTtlDuration(long ttlDuration) {
        this.ttlDuration = ttlDuration;
    }

    public String getTtlCol() {
        return ttlCol;
    }

    public void setTtlCol(String ttlCol) {
        this.ttlCol = ttlCol;
    }

}