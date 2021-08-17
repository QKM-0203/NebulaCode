/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import java.util.ArrayList;
import java.util.HashMap;
import org.junit.Test;

public class TestDelete extends TestDataBase {
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
        graph.createTag(qkm6);
        assert graph.getTags().contains("\"" + qkm6.getName() + "\"");
        ArrayList<String> tagList = new ArrayList<>();
        tagList.add("QKM6");
        graph.dropTagList(tagList);
        graph.dropTag("QKM6");
        assert !graph.getTags().contains("\"" + qkm6.getName() + "\"");
    }

    @Test
    public void deleteEdge() {
        graph.createEdge(qkm5);
        assert graph.getTags().contains("\"" + qkm5.getName() + "\"");
        ArrayList<String> edgeList = new ArrayList<>();
        edgeList.add("QKM5");
        graph.dropEdgeList(edgeList);
        graph.dropEdge("QKM5");
        assert !graph.getEdges().contains("\"" + qkm5.getName() + "\"");
    }

    @Test
    public void deleteEdgeIndex() {
        HashMap<String, Integer> edgeIndexes = new HashMap<>();
        edgeIndexes.put("money", 10);
        edgeIndexes.put("number", null);
        graph.createEdgeIndex("QKM5", "t_qkm5", edgeIndexes);
        assert graph.getEdgeIndexes("QKM5").contains("\"t_qkm5\"");
        graph.dropEdgeIndex("t_qkm5");
        assert !graph.getEdgeIndexes("QKM5").contains("\"t_qkm5\"");
    }

    @Test
    public void deleteTagIndex() {
        HashMap<String, Integer> tagIndexes = new HashMap<>();
        graph.createTagIndex("QKM6", "t_qkm6", null);
        assert graph.getTagIndexes("QKM6").contains("\"t_qkm6\"");
        graph.dropTagIndex("t_qkm6");
        assert !graph.getTagIndexes("QKM6").contains("\"t_qkm6\"");
    }

}
