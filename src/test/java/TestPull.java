/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import com.vesoft.nebula.orm.entity.Relationship;
import com.vesoft.nebula.orm.entity.Vertex;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import org.junit.Test;

public class TestPull extends TestDataBase {
    @Test
    public void testCanNotPullOtherObject() {
        StringBuffer qkm = new StringBuffer("qkm");
        try {
            graph.pull(qkm);
        } catch (Exception e) {
            assert e.getMessage().equals("java.lang.StringBuffer object is not support");
        }
    }

    @Test
    public void testCanPullVertex() throws UnsupportedEncodingException {
        HashMap<String, HashMap<String, Object>> propMap = new HashMap<>();
        propMap.put("QKM5", null);
        Vertex vertex = new Vertex(null, propMap);
        vertex.setVid(vertexOne.getVid());
        graph.create(vertexOne);
        graph.pull(vertex);
        assert vertex.getTagNames().equals(vertexOne.getTagNames());
        assert !vertex.getTagNames().contains("QKM5");
    }

    @Test
    public void testPullRelationship() throws UnsupportedEncodingException {
        HashMap<String, Object> edgeValue = new HashMap<>();
        edgeValue.put("object", "PE");
        Relationship relationship = new Relationship(1, 2, "team", edgeValue, 1);
        graph.create(relationship12);
        graph.pull(relationship);
        assert relationship.getPropMap().get("object").toString().equals("math");
    }

    @Test
    public void testPullSubgraph() throws UnsupportedEncodingException {
        graph.create(subgraph);
        graph.run("upsert edge on team 1->2@1 set teacherName = \"XuXin\"");
        graph.run("upsert vertex on QKM2 3 set name = \"wm\"");
        graph.pull(subgraph);
        assert subgraph.getVertexes().get(2).getPropMap().get("QKM2")
            .get("name").toString().equals("wm");
        assert subgraph.getRelationships().get(0).getPropMap()
            .get("teacherName").toString().equals("XuXin");
    }

}
