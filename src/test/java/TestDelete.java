/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.orm.entity.Schema;
import java.util.ArrayList;
import java.util.HashMap;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestDelete extends TestDataBase {

    {
        graph.createTag(qkm1);
        graph.createTag(qkm2);
        graph.createEdge(team);
        graph.createEdge(work);
    }

    @Test
    public void testDropSpace() {
        ArrayList<String> spaceNameList = new ArrayList<>();
        spaceNameList.add("test1");
        spaceNameList.add("test2");
    }

    @Test
    public void testDropVertex() {
        graph.create(vertexTwo);
        assert graph.exists(vertexTwo);
        graph.delete(vertexTwo);
        assert !graph.exists(vertexTwo);
    }

    @Test
    public void testDropRelationship() {
        graph.create(relationship12);
        assert graph.exists(relationship12);
        graph.delete(relationship12);
        assert !graph.exists(relationship12);
    }

    @Test
    public void testDropSubgraph() {
        graph.create(subgraph);
        assert graph.exists(subgraph);
        graph.delete(subgraph);
        assert !graph.exists(subgraph);
    }

    @Test
    public void testDropPath() {
        graph.create(path);
        assert graph.exists(path);
        graph.delete(path);
        assert !graph.exists(path);
    }

    @Test
    public void deleteTag() {
        Schema tag = new Schema("QKM3", tagProperties, 0, null);
        graph.createTag(tag);
        assert graph.getTags().contains(tag.getName());
        ArrayList<String> tagList = new ArrayList<>();
        tagList.add("QKM3");
        graph.dropTagIndex("i_QKM3");//first delete tagIndex
        graph.dropTagList(tagList);
        graph.dropTag("QKM3");
        assert !graph.getTags().contains(tag.getName());
    }

    @Test
    public void deleteEdge() {
        Schema edge = new Schema("QKM4", edgeProperties, 0, null);
        graph.createEdge(edge);
        assert graph.getEdges().contains(edge.getName());
        ArrayList<String> edgeList = new ArrayList<>();
        edgeList.add("QKM4");
        graph.dropEdgeIndex("i_QKM4_teacherName_object");//first delete edgeIndex
        graph.dropEdgeList(edgeList);
        graph.dropEdge("QKM4");
        assert !graph.getEdges().contains(edge.getName());
    }

    @Test
    public void deleteEdgeIndex() {
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("teacherName", 10);
        edgeIndexes.put("object", 10);
        Schema edge = new Schema("QKM4", edgeProperties, 0, null);
        graph.createEdge(edge);
        graph.createEdgeIndex("QKM4", "i_QKM4_teacherName_object", edgeIndexes);
        assert graph.getEdgeIndexes("QKM4").contains("i_QKM4_teacherName_object");
        graph.dropEdgeIndex("i_QKM4_teacherName_object");
        assert !graph.getEdgeIndexes("QKM4").contains("i_QKM4_teacherName_object");
    }

    @Test
    public void deleteTagIndex() {
        Schema tag = new Schema("QKM3", tagProperties, 0, null);
        graph.createTag(tag);
        graph.createTagIndex("QKM3", "i_QKM3", null);
        assert graph.getTagIndexes("QKM3").contains("i_QKM3");
        graph.dropTagIndex("i_QKM3");
        assert !graph.getTagIndexes("QKM3").contains("i_QKM3");
    }

}
