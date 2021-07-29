/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package entity;

import operator.DataType;

/**
 * you can set the property name, property value type, and property value settings for a property.
 *
 * <p>{@link #propName} is attribute name,{@link #dataType} is attribute type,
 * {@link #isNullable} whether is null,{@link #defaultValue} attribute default value.</p>
 *
 * @author Qi Kai Meng
 */
public class Property {
    private  String propName;
    private DataType dataType;
    private  boolean isNullable;
    private  Object defaultValue;

    /**
     * user pass in propName、dataType、isNullable、defaultValue can create
     * a satisfactory attribute.
     *
     * @param propName attribute name
     * @param dataType  attribute data type
     * @param isNullable whether is null
     * @param defaultValue default value
     */
    public Property(String propName, DataType dataType, boolean isNullable, Object defaultValue) {
        this.propName = propName;
        this.dataType = dataType;
        this.isNullable = isNullable;
        this.defaultValue = defaultValue;
    }

    public String getPropName() {
        return propName;
    }

    public void setPropName(String propName) {
        this.propName = propName;
    }

    public DataType getDataType() {
        return dataType;
    }

    public void setDataType(DataType dataType) {
        this.dataType = dataType;
    }

    public boolean isNullable() {
        return isNullable;
    }

    public void setNullable(boolean nullable) {
        isNullable = nullable;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(Object defaultValue) {
        this.defaultValue = defaultValue;
    }
}
