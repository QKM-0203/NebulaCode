/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import java.util.List;

public class Subgraph {
    private List<Relationship> relationshipsList;


    public Subgraph(List<Relationship> relationshipsList) {
        this.relationshipsList = relationshipsList;
    }

    public void setRelationshipsList(List<Relationship> relationshipsList) {
        this.relationshipsList = relationshipsList;
    }
}
