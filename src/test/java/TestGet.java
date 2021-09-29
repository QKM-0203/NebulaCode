/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

import java.io.UnsupportedEncodingException;
import java.util.List;
import org.junit.Test;

/**
 * because the schema creation and index creation steps are implemented asynchronously,
 * the Nepal graph cannot be created until the next heartbeat cycle,
 * so you can wait and execute the method again.
 */
public class TestGet extends TestDataBase {
    {
        graph.createTag(person);
        graph.createTag(hobby);
        graph.createEdge(subject);
        graph.createEdge(work);
        graph.create(vertexOne);
        graph.create(vertexTwo);
        graph.create(vertexThird);
        graph.create(vertexFour);
        graph.create(relationship12);
        graph.create(relationship32);
        graph.create(relationship24);
        graph.create(relationship34);
        graph.create(relationship23);
        graph.create(relationship13);
    }

    @Test
    public void testGetTags() {
        List<String> tags = graph.getTags();
        assert !tags.isEmpty();
    }

    @Test
    public void testGetEdges() {
        List<String> tags = graph.getEdges();
        assert !tags.isEmpty();
    }

    @Test//you first execute submit job stats
    public void testGetAllVertexNumber() throws UnsupportedEncodingException {
        long number = graph.vertexNumber(null);
        assert number == 4;
    }

    @Test//you first execute submit job stats
    public void testGetDesignatedVertexNumber() throws UnsupportedEncodingException {
        long number = graph.vertexNumber("hobby");
        assert number == 4;
    }

    @Test//you first execute submit job stats
    public void testGetAllDesignatedEdgesNumber() throws UnsupportedEncodingException {
        long number = graph.relationshipNumber("subject");
        assert number == 3;
    }
}
