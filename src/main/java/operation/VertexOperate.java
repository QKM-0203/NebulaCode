/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package operation;

import common.Operate;
import entity.Graph;
import entity.Schema;
import entity.Space;
import entity.Vertex;

import java.util.List;

public class VertexOperate implements Operate<Vertex> {
    private final Graph graph;

    public VertexOperate(Graph graph ){
        this.graph = graph;
    }

    @Override
    public void create(Vertex Entity) throws NoSuchFieldException, IllegalAccessException {

    }

    @Override
    public void drop(Vertex Entity) throws NoSuchFieldException, IllegalAccessException {

    }

    public Space getSpace(){
        return null;
    }

    public List<Schema> getTags(){
        return null;
    }

    public boolean isHasTag(String tagName){
        return true;
    }

    public boolean addTag(Schema tag){
        return true;
    }

    public boolean removeTag(String tagName){
        return true;
    }

    public boolean clearAllTags(){
        return true;
    }

    public boolean updateTag(Schema tag){
        return true;
    }


}
