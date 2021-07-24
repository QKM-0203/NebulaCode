/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import java.util.List;

public class Schema {

    private final String name;

    //propertyList
    private List<Property> propertyList;


    //timestamp difference default value is zero
    private long ttl_duration = 0;


    //to determine which attribute to set the lifetime, the type must be int or timestamp
    private String ttl_col;


    /**
     *
     * Create an Tag or EdgeType of include property
     * @param name tag name or edgeType name
     * @param propertyList  Property List
     */
    public Schema(String name, List<Property> propertyList){
        this.name = name;
        this.propertyList = propertyList;
    }

    /**
     * create an Tag or EdgeType of include property and property time setting
     * @param name Tag name or EdgeType name
     * @param propertyList property List
     * @param ttl_duration timestamp difference
     * @param ttl_col set the property name of the lifetime
     */
    public Schema(String name, List<Property> propertyList, long ttl_duration, String ttl_col) {
        this.name = name;
        this.propertyList = propertyList;
        this.ttl_duration = ttl_duration;
        this.ttl_col = ttl_col;
    }

    /**
     * create an empty property
     * @param name Tag name or EdgeType name
     */
    public Schema(String name){
        this.name = name;
    }


    public void setTtl_duration(long ttl_duration) {
        this.ttl_duration = ttl_duration;
    }


    public void setTtl_col(String ttl_col) {
        this.ttl_col = ttl_col;
    }


    public void setPropertyList(List<Property> propertyList) {
        this.propertyList = propertyList;
    }


}
