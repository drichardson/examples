#import <Foundation/Foundation.h>
#include <svn_client.h>
#include <svn_repos.h>

static int
make_new_directory(const char *repos_path,
                   const char *new_directory,
                   apr_pool_t *pool);

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    // insert code here...
    NSLog(@"Hello, World!");
    [pool drain];
    return 0;
}

/* Convert a Subversion error into a simple boolean error code.
*
* NOTE:  Subversion errors must be cleared (using svn_error_clear())
*        because they are allocated from the global pool, else memory
*        leaking occurs.
*/
#define INT_ERR(expr)                           \
do {                                          \
svn_error_t *__temperr = (expr);            \
if (__temperr)                              \
{                                         \
svn_error_clear(__temperr);             \
return 1;                               \
}                                         \
return 0;                                   \
} while (0)

/* Create a new directory at the path NEW_DIRECTORY in the Subversion
 * repository located at REPOS_PATH.  Perform all memory allocation in
 * POOL.  This function will create a new revision for the addition of
 * NEW_DIRECTORY.  Return zero if the operation completes
 * successfully, nonzero otherwise.
 */
static int
make_new_directory(const char *repos_path,
                   const char *new_directory,
                   apr_pool_t *pool)
{
	svn_error_t *err;
	svn_repos_t *repos;
	svn_fs_t *fs;
	svn_revnum_t youngest_rev;
	svn_fs_txn_t *txn;
	svn_fs_root_t *txn_root;
	const char *conflict_str;
	
	/* Open the repository located at REPOS_PATH. 
	 */
	INT_ERR(svn_repos_open(&repos, repos_path, pool));
	
	/* Get a pointer to the filesystem object that is stored in REPOS. 
	 */
	fs = svn_repos_fs(repos);
	
	/* Ask the filesystem to tell us the youngest revision that
	 * currently exists. 
	 */
	INT_ERR(svn_fs_youngest_rev(&youngest_rev, fs, pool));
	
	/* Begin a new transaction that is based on YOUNGEST_REV.  We are
	 * less likely to have our later commit rejected as conflicting if we
	 * always try to make our changes against a copy of the latest snapshot
	 * of the filesystem tree. 
	 */
	INT_ERR(svn_repos_fs_begin_txn_for_commit2(&txn, repos, youngest_rev,
											   apr_hash_make(pool), pool));
	
	/* Now that we have started a new Subversion transaction, get a root
	 * object that represents that transaction. 
	 */
	INT_ERR(svn_fs_txn_root(&txn_root, txn, pool));
	
	/* Create our new directory under the transaction root, at the path
	 * NEW_DIRECTORY. 
	 */
	INT_ERR(svn_fs_make_dir(txn_root, new_directory, pool));
	
	/* Commit the transaction, creating a new revision of the filesystem
	 * which includes our added directory path.
	 */
	err = svn_repos_fs_commit_txn(&conflict_str, repos, 
								  &youngest_rev, txn, pool);
	if (! err)
    {
		/* No error?  Excellent!  Print a brief report of our success.
		 */
		printf("Directory '%s' was successfully added as new revision "
			   "'%ld'.\n", new_directory, youngest_rev);
    }
	else if (err->apr_err == SVN_ERR_FS_CONFLICT)
    {
		/* Uh-oh.  Our commit failed as the result of a conflict
		 * (someone else seems to have made changes to the same area 
		 * of the filesystem that we tried to modify).  Print an error
		 * message.
		 */
		printf("A conflict occurred at path '%s' while attempting "
			   "to add directory '%s' to the repository at '%s'.\n", 
			   conflict_str, new_directory, repos_path);
    }
	else
    {
		/* Some other error has occurred.  Print an error message.
		 */
		printf("An error occurred while attempting to add directory '%s' "
			   "to the repository at '%s'.\n", 
			   new_directory, repos_path);
    }
	
	INT_ERR(err);
} 