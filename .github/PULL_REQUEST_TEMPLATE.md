# Checklist

- Branch
    - [ ] Commit sequence broadly makes sense
    - [ ] Commits have useful messages
    - [ ] New tests are added if needed and existing tests are updated
    - [ ] If this branch changes Consensus and has any consequences for downstream repositories or end users, said changes must be documented in [`interface-CHANGELOG.md`](../ouroboros-consensus/docs/interface-CHANGELOG.md)
    - [ ] If this branch changes Network and has any consequences for downstream repositories or end users, said changes must be documented in [`interface-CHANGELOG.md`](../docs/interface-CHANGELOG.md)    
- Pull Request
    - [ ] Self-reviewed the diff
    - [ ] Useful pull request description at least containing the following information:
      - What does this PR change?
      - Why these changes were needed?
      - How does this affect downstream repositories and/or end-users?
      - Which ticket does this PR close (if any)? If it does, is it [linked](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue)?
    - [ ] Reviewer requested
