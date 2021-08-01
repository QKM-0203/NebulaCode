/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.entity;

import com.vesoft.nebula.client.graph.data.ResultSet;
import com.vesoft.nebula.orm.exception.ExecuteException;
import com.vesoft.nebula.orm.exception.InitException;
import com.vesoft.nebula.orm.ngql.Encoding;
import java.util.ArrayList;
import java.util.List;

/**
 * create tag object or edgeType object.
 *
 * <p>{@link #name} is tagName or edgeTypeName,{@link #propertyList} is attribute list,
 * you can also set a specific expiration timestamp for a property.</p>
 *
 * @author Qi Kai Meng
 */
public class Schema extends Entity {
    private final String name;
    private List<Property> propertyList;
    private long ttlDuration = 0;
    private String ttlCol;
    private int flag = 0;

    /**
     * create an Tag or EdgeType of include property.
     *
     * @param name tag name or edgeName
     * @param propertyList  Property List
     * @param flag  if create tag pass in 0,otherwise pass in 1,default create tag
     */
    public Schema(String name, List<Property> propertyList,int flag) {
        this.name = name;
        this.propertyList = propertyList;
        this.flag = flag;
    }

    /**
     * create an Tag or EdgeType of include property and property time setting.
     *
     * @param name Tag name or EdgeType name
     * @param propertyList property List
     * @param ttlDuration timestamp difference,timestamp difference default value is zero
     *                    symbol never expire
     * @param ttlCol to determine which attribute to set the lifetime,
     *               the type must be int or timestamp,if pass in null, symbol not set
     * @param flag  tag or edge,flag == 0 is tag
     */
    public Schema(String name, List<Property> propertyList,
                  long ttlDuration, String ttlCol,int flag) {
        this.name = name;
        this.propertyList = propertyList;
        this.ttlDuration = ttlDuration;
        this.ttlCol = ttlCol;
        this.flag = flag;
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

    public int getFlag() {
        return flag;
    }

    public void setFlag(int flag) {
        this.flag = flag;
    }

    @Override
    public Graph getGraph() {
        return super.getGraph();
    }

    /**
     * add attribute for schema(tag or edge)
     * @param propertyList propList of add
     */
    public void addProp(List<Property> propertyList) {
        if (propertyList == null || propertyList.isEmpty()) {
            throw new InitException("propertyList is null or empty");
        }
        ResultSet resultSet = getGraph().run(String.format(Encoding.propJoin(propertyList),
            this.getFlag() == 0 ? "TAG" : "EDGE", this.getName(),"ADD"));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        this.propertyList.addAll(propertyList);
    }


    /**
     *
     * @param ttlDuration timestamp difference,timestamp difference default value is zero
     *      *                    symbol never expire
     * @param ttlCol to determine which attribute to set the lifetime,
     *      *               the type must be int or timestamp,if pass in null, symbol not set
     */
    public void addTtlForProp(long ttlDuration, String ttlCol) {
        ResultSet resultSet = null;
        if (ttlCol != null) {
            resultSet = getGraph().run(
                String.format("ALTER %s %s TTL_DURATION = %d,TTL_COL = %s",
                    this.getFlag() == 0 ? "TAG" : "EDGE", "`" + this.getName()
                        + "`", ttlDuration, ttlCol));
        } else {
            resultSet = getGraph().run(
                String.format("ALTER %s %s TTL_DURATION = %d",
                    this.getFlag() == 0 ? "TAG" : "EDGE", "`" + this.getName()
                        + "`", ttlDuration));
        }
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }

    }


    /**
     * delete specific attributes of a schema pass in attribute name List
     * @param propNames attribute name List
     */
    public void delProp(List<String> propNames) {
        if (propNames == null || propNames.isEmpty()) {
            throw new InitException("propNames is null or empty");
        }
        ResultSet resultSet = getGraph().run(String.format(Encoding.delPropJoin(propNames),
            this.getFlag() == 0 ? "TAG" : "EDGE", this.getName()));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        propertyList.removeIf(property -> propNames.contains(property.getPropName()));
    }

    /**
     * modify specific attributes of a schema pass in attribute name List
     * @param propertyList propList if propertyList is null or empty,is nothing to do
     */
    public void modifyProp(List<Property> propertyList) {
        if (propertyList == null || propertyList.isEmpty()) {
            return;
        }
        ResultSet resultSet = getGraph().run(String.format(Encoding.propJoin(propertyList),
            this.getFlag() == 0 ? "TAG" : "EDGE", this.getName(),"CHANGE"));
        if (!resultSet.isSucceeded()) {
            throw new ExecuteException(resultSet.getErrorMessage());
        }
        ArrayList<String> propNames = new ArrayList<>();
        for (Property property : propertyList) {
            propNames.add(property.getPropName());
        }
        this.propertyList.removeIf(property -> propNames.contains(property.getPropName()));
        this.propertyList.addAll(propertyList);
    }


    /**
     * get attribute type
     * @param propName attribute name
     */
    public String getPropType(String propName) {
        List<Property> propertyList = this.getPropertyList();
        for (Property property : propertyList) {
            if (propName.equals(property.getPropName())) {
                return property.getDataType().toString();
            }
        }
        return null;
    }
}