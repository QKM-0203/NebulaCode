/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.ngql;

/**
 * alias properties
 */
public class AttributeColumn {
    private String propName;
    private String alias;


    public AttributeColumn(String propName, String alias) {
        this.propName = propName;
        this.alias = alias;
    }

    public String getPropName() {
        return propName;
    }

    public String getAlias() {
        return alias;
    }

}
