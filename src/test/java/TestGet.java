/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */


import java.util.List;
import org.junit.Test;

public class TestGet extends Data {
    @Test
    public void testGetTags() {
        List<String> tags = graph.getTags();
        assert tags.toString().equals("[QKM1, QKM2, QKM3, QKM4, "
            + "QKM6]");
    }

    @Test
    public void testGetEdges() {
        List<String> tags = graph.getEdges();
        assert tags.toString().equals("[QKM5, team, work]");
    }

}
