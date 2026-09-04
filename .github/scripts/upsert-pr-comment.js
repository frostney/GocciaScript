// Report builders own the body and markers; CI owns the token and permissions.
module.exports = async ({ github, context, body, markers }) => {
  const issue = { ...context.repo, issue_number: context.issue.number };
  const { data: comments } = await github.rest.issues.listComments(issue);
  const existing = comments.find(comment =>
    comment.body && markers.some(marker => comment.body.includes(marker)));

  if (existing) {
    await github.rest.issues.updateComment({
      ...context.repo, comment_id: existing.id, body,
    });
  } else {
    await github.rest.issues.createComment({ ...issue, body });
  }
};
